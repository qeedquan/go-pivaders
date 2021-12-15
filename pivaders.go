package main

import (
	"flag"
	"fmt"
	"image"
	"image/color"
	"image/draw"
	"math/rand"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"time"

	"github.com/qeedquan/go-media/image/imageutil"
	"github.com/qeedquan/go-media/math/ga"
	"github.com/qeedquan/go-media/sdl"
	"github.com/qeedquan/go-media/sdl/sdlgfx"
	"github.com/qeedquan/go-media/sdl/sdlimage/sdlcolor"
	"github.com/qeedquan/go-media/sdl/sdlmixer"
	"github.com/qeedquan/go-media/sdl/sdlttf"
)

var (
	Res         = ga.Vec2d{800, 600}
	BulletSize  = ga.Vec2d{5, 10}
	MissileSize = ga.Vec2d{5, 5}
	BlockSize   = ga.Vec2d{10, 10}
	AlienSize   = ga.Vec2d{30, 40}
)

const (
	AlienSpacer   = 20
	BarrierRow    = 10
	BarrierColumn = 4
)

const (
	StateTitle = iota
	StatePlay
	StateBreached
	StateLost
	StateWon
)

var (
	conf struct {
		assets     string
		fullscreen bool
		music      bool
		sound      bool
		invincible bool
	}

	window   *sdl.Window
	renderer *sdl.Renderer
	texture  *sdl.Texture
	surface  *sdl.Surface
	canvas   *image.RGBA
	fps      sdlgfx.FPSManager

	ctls []*sdl.GameController

	images     = make(map[string]*Image)
	intro      *Image
	background *Image
	font       *sdlttf.Font
	bfont      *sdlttf.Font

	bgm *sdlmixer.Music
	sfx struct {
		bullet    *sdlmixer.Chunk
		explosion *sdlmixer.Chunk
	}

	state    int
	ticker   *time.Ticker
	gtime    uint32
	ltime    uint32
	paused   bool
	player   *Player
	barriers []*Barrier
	bullets  []*Ammo
	aliens   []*Alien
	missiles []*Ammo
)

func main() {
	runtime.LockOSThread()
	parseFlags()
	initSDL()
	loadAssets()

	playMusic(bgm)
	state = StateTitle
	for {
		gtime = sdl.GetTicks()
		event()
		update()
		blit()
		fps.Delay()
	}
}

func parseFlags() {
	conf.assets = filepath.Join(sdl.GetBasePath(), "assets")
	flag.StringVar(&conf.assets, "assets", conf.assets, "assets directory")
	flag.BoolVar(&conf.fullscreen, "fullscreen", false, "fullscreen mode")
	flag.BoolVar(&conf.music, "music", true, "enable music")
	flag.BoolVar(&conf.sound, "sound", true, "enable sound")
	flag.BoolVar(&conf.invincible, "invincible", false, "be invincible")
	flag.Parse()
}

func initSDL() {
	err := sdl.Init(sdl.INIT_EVERYTHING &^ sdl.INIT_AUDIO)
	ck(err)

	err = sdlttf.Init()
	ck(err)

	err = sdl.InitSubSystem(sdl.INIT_AUDIO)
	ek(err)

	err = sdlmixer.OpenAudio(44100, sdl.AUDIO_S16, 2, 8192)
	ek(err)

	_, err = sdlmixer.Init(sdlmixer.INIT_OGG)
	ek(err)

	sdlmixer.AllocateChannels(128)

	sdl.SetHint(sdl.HINT_RENDER_SCALE_QUALITY, "best")

	w, h := int(Res.X), int(Res.Y)
	wflag := sdl.WINDOW_RESIZABLE
	if conf.fullscreen {
		wflag |= sdl.WINDOW_FULLSCREEN_DESKTOP
	}
	window, renderer, err = sdl.CreateWindowAndRenderer(w, h, wflag)
	ck(err)

	texture, err = renderer.CreateTexture(sdl.PIXELFORMAT_ABGR8888, sdl.TEXTUREACCESS_STREAMING, w, h)
	ck(err)

	surface, err = sdl.CreateRGBSurface(sdl.SWSURFACE, w, h, 32, 0x00FF0000, 0x0000FF00, 0x000000FF, 0xFF000000)
	ck(err)

	canvas = image.NewRGBA(image.Rect(0, 0, w, h))

	window.SetTitle("Pivaders")
	renderer.SetLogicalSize(w, h)

	mapControllers()

	sdl.ShowCursor(0)

	fps.Init()
	fps.SetRate(60)
}

func mapControllers() {
	for _, c := range ctls {
		if c != nil {
			c.Close()
		}
	}

	ctls = make([]*sdl.GameController, sdl.NumJoysticks())
	for i, _ := range ctls {
		if sdl.IsGameController(i) {
			var err error
			ctls[i], err = sdl.GameControllerOpen(i)
			ek(err)
		}
	}
}

func loadAssets() {
	intro = loadImage("graphics/start_screen.jpg", color.Transparent)
	background = loadImage("graphics/Space-Background.jpg", color.Transparent)

	font = loadFont(28)
	bfont = loadFont(72)

	bgm = loadMusic("sound/10_Arpanauts.ogg")
	sfx.bullet = loadSound("sound/medetix__pc-bitcrushed-lazer-beam.ogg")
	sfx.explosion = loadSound("sound/timgormly__8-bit-explosion.ogg")
}

func loadImage(name string, key color.Color) *Image {
	name = filepath.Join(conf.assets, name)
	if p := images[name]; p != nil {
		return p
	}

	m, err := imageutil.LoadRGBAFile(name)
	ck(err)
	m = imageutil.ColorKey(m, key)

	r := m.Bounds()
	p := &Image{m, r.Dx(), r.Dy()}
	images[name] = p
	return p
}

func loadFont(ptSize int) *sdlttf.Font {
	name := filepath.Join(conf.assets, "Orbitracer.ttf")
	font, err := sdlttf.OpenFont(name, ptSize)
	ck(err)
	return font
}

func loadMusic(name string) *sdlmixer.Music {
	name = filepath.Join(conf.assets, name)
	mus, err := sdlmixer.LoadMUS(name)
	ek(err)
	return mus
}

func loadSound(name string) *sdlmixer.Chunk {
	name = filepath.Join(conf.assets, name)
	chunk, err := sdlmixer.LoadWAV(name)
	ek(err)
	return chunk
}

func event() {
	for {
		ev := sdl.PollEvent()
		if ev == nil {
			break
		}

		switch ev := ev.(type) {
		case sdl.QuitEvent:
			os.Exit(0)

		case sdl.KeyDownEvent:
			switch ev.Sym {
			case sdl.K_ESCAPE:
				if state == StateTitle {
					os.Exit(0)
				}
				sdlmixer.ResumeMusic()
				state = StateTitle
			case sdl.K_RETURN:
				if state == StatePlay {
					togglePause()
				}
			case sdl.K_BACKSPACE:
				toggleInvincibility()
			}

		case sdl.ControllerButtonDownEvent:
			button := sdl.GameControllerButton(ev.Button)
			switch button {
			case sdl.CONTROLLER_BUTTON_BACK:
				if state == StateTitle {
					os.Exit(0)
				}
				sdlmixer.ResumeMusic()
				state = StateTitle
			case sdl.CONTROLLER_BUTTON_START:
				if state == StatePlay {
					togglePause()
				}
			}

		case sdl.ControllerDeviceAddedEvent:
			mapControllers()
		}
	}

	key := sdl.GetKeyboardState()
	switch state {
	case StateTitle:
		if key[sdl.SCANCODE_SPACE] != 0 {
			reset(0, 1, 0)
		}
		for _, c := range ctls {
			if c == nil {
				continue
			}

			if c.Button(sdl.CONTROLLER_BUTTON_START) != 0 {
				reset(0, 1, 0)
			}
		}

	case StatePlay:
		p := player
		switch {
		case key[sdl.SCANCODE_LEFT] != 0:
			p.Vector.X = -1
			p.Anim = 0x2
		case key[sdl.SCANCODE_RIGHT] != 0:
			p.Vector.X = 1
			p.Anim = 0x1
		default:
			p.Vector.X = 0
			p.Anim = 0
		}

		if key[sdl.SCANCODE_SPACE] != 0 {
			p.Shoot = true
		}

		for _, c := range ctls {
			if c == nil {
				continue
			}

			const threshold = 1000
			switch {
			case c.Button(sdl.CONTROLLER_BUTTON_DPAD_LEFT) != 0,
				c.Axis(sdl.CONTROLLER_AXIS_LEFTX) < -threshold,
				c.Axis(sdl.CONTROLLER_AXIS_RIGHTX) < -threshold:
				p.Vector.X = -1
				p.Anim = 0x2
			case c.Button(sdl.CONTROLLER_BUTTON_DPAD_RIGHT) != 0,
				c.Axis(sdl.CONTROLLER_AXIS_LEFTX) > threshold,
				c.Axis(sdl.CONTROLLER_AXIS_RIGHTX) > threshold:
				p.Vector.X = 1
				p.Anim = 0x1
			}

			switch {
			case c.Button(sdl.CONTROLLER_BUTTON_X) != 0,
				c.Button(sdl.CONTROLLER_BUTTON_Y) != 0,
				c.Button(sdl.CONTROLLER_BUTTON_A) != 0,
				c.Button(sdl.CONTROLLER_BUTTON_B) != 0:
				p.Shoot = true
			}
		}
	}
}

func togglePause() {
	paused = !paused
	if paused {
		sdlmixer.PauseMusic()
		player.Pause()
		for _, b := range bullets {
			b.Pause()
		}
		for _, b := range missiles {
			b.Pause()
		}
		for _, a := range aliens {
			a.Pause()
		}
	} else {
		sdlmixer.ResumeMusic()
		player.Unpause()
		for _, b := range bullets {
			b.Unpause()
		}
		for _, b := range missiles {
			b.Unpause()
		}
		for _, a := range aliens {
			a.Unpause()
		}
	}
}

func toggleInvincibility() {
	conf.invincible = !conf.invincible
	sdl.Log("invincibility set to %v", conf.invincible)
}

func reset(score uint64, round, speed uint32) {
	state = StatePlay
	paused = false
	player = newPlayer()
	barriers = makeDefenses()
	aliens = makeAliens(speed)
	bullets = bullets[:0]
	missiles = missiles[:0]
	sdlmixer.ResumeMusic()
	if ticker != nil {
		ticker.Stop()
	}
	ticker = time.NewTicker(50 * time.Millisecond)

	player.Score = score
	player.Round = round
}

func update() {
	switch {
	case state == StateBreached || state == StateLost:
		if gtime-ltime > 3000 {
			state = StateTitle
		}
		return

	case state == StateWon:
		if gtime-ltime > 3000 {
			speed := uint32(player.Round)
			if speed > 16 {
				speed = 16
			}
			speed *= 50
			reset(player.Score, player.Round, speed)
		}
		return

	case state == StateTitle || paused:
		return
	}

	select {
	case <-ticker.C:
	default:
		return
	}

	if defensesBreached() || isDead() || wonRound() {
		ltime = sdl.GetTicks()
		return
	}

	collision()

	player.Update()
	if player.Shoot {
		makeBullet()
	}
	for _, a := range aliens {
		a.Update()
	}
	makeMissile()
	for _, b := range bullets {
		b.Update()
	}
	for _, m := range missiles {
		m.Update()
	}
}

func isDead() bool {
	if player.Dead {
		state = StateLost
		return true
	}
	return false
}

func defensesBreached() bool {
	for _, a := range aliens {
		if a.Y > 410 {
			state = StateBreached
			return true
		}
	}
	return false
}

func wonRound() bool {
	if len(aliens) == 0 {
		if player.Round < 1e9 {
			player.Round++
		}
		state = StateWon
		return true
	}
	return false
}

func collision() {
	ammoCollision(bullets, barriers, 'b')
	ammoCollision(missiles, barriers, 'b')
	ammoCollision(bullets, aliens, 'a')
	ammoCollision(missiles, player, 'p')

	reap(&bullets)
	reap(&missiles)
	reap(&barriers)
	reap(&aliens)
}

func ammoCollision(ammo []*Ammo, entity interface{}, typ int) {
	f := func(a *Ammo, v reflect.Value) {
		e := v.FieldByName("Entity").Addr().Interface().(*Entity)
		if collide(&a.Entity, e) {
			a.Dead = true
			if conf.invincible && typ == 'p' {
				return
			}

			if typ == 'a' {
				player.Frag()
				playSound(sfx.explosion, 1)
				alien := v.Addr().Interface().(*Alien)
				alien.ExpPos = ga.Vec2d{alien.X, alien.Y}
			} else if typ == 'p' {
				playSound(sfx.explosion, 1)
			}

			if typ != 'b' {
				v.FieldByName("Exploding").SetBool(true)
			} else {
				v.FieldByName("Dead").SetBool(true)
			}
		}
	}

	v := reflect.ValueOf(entity)
	for _, a := range ammo {
		if a.Dead {
			continue
		}
		switch v.Kind() {
		case reflect.Slice:
			for i := 0; i < v.Len(); i++ {
				e := v.Index(i).Elem()
				f(a, e)
			}

		case reflect.Ptr:
			f(a, v.Elem())

		default:
			panic("unreachable")
		}
	}
}

func reap(x interface{}) {
	v := reflect.ValueOf(x)
	s := v.Elem()
	for i := 0; i < s.Len(); i++ {
		p := s.Index(i)
		dead := p.Elem().FieldByName("Dead").Interface().(bool)
		if dead {
			q := s.Index(s.Len() - 1)
			p.Set(q)
			s.Set(s.Slice(0, s.Len()-1))
		}
	}
}

func blit() {
	switch state {
	case StateTitle:
		intro.Blit(0, 0, draw.Src)
		blitText(bfont, sdlcolor.White, 265, 120, "PIVADERS")
		blitText(font, sdlcolor.White, 274, 191, "PRESS SPACE TO PLAY")

	default:
		background.Blit(0, 0, draw.Src)
		player.Blit()
		for _, b := range barriers {
			b.Blit()
		}
		for _, a := range aliens {
			a.Blit()
		}
		for _, b := range bullets {
			b.Blit()
		}
		for _, m := range missiles {
			m.Blit()
		}
		blitText(font, sdlcolor.White, 10, 8, fmt.Sprint("SCORE ", player.Score))
		blitText(font, sdlcolor.Red, 10, 35, fmt.Sprint("LIVES ", player.Lives))
		if paused {
			w, h, _ := bfont.SizeUTF8("PAUSED")
			x := int((Res.X - float64(w)) / 2)
			y := int((Res.Y - float64(h)) / 2)
			blitText(bfont, sdlcolor.White, x, y, "PAUSED")
		}
	}

	switch state {
	case StateBreached:
		r, err := font.RenderUTF8BlendedEx(surface, "The aliens have breached Earth defenses!", sdlcolor.Red)
		ck(err)
		x := 180
		y := 15
		draw.Draw(canvas, image.Rect(x, y, x+int(r.W), y+int(r.H)), surface, image.ZP, draw.Over)

	case StateLost:
		text := fmt.Sprintf("The war is lost! You scored: %v", player.Score)
		r, err := font.RenderUTF8BlendedEx(surface, text, sdlcolor.Red)
		ck(err)
		x := 250
		y := 151
		draw.Draw(canvas, image.Rect(x, y, x+int(r.W), y+int(r.H)), surface, image.ZP, draw.Over)

	case StateWon:
		text := fmt.Sprintf("You won round %v but the battle rages on", player.Round-1)
		r, err := font.RenderUTF8BlendedEx(surface, text, sdlcolor.Red)
		ck(err)
		x := 200
		y := 15
		draw.Draw(canvas, image.Rect(x, y, x+int(r.W), y+int(r.H)), surface, image.ZP, draw.Over)

	}

	renderer.SetDrawColor(sdlcolor.Black)
	renderer.Clear()

	frame, err := texture.Lock(nil)
	ck(err)
	copy(frame, canvas.Pix)
	texture.Unlock()
	renderer.Copy(texture, nil, nil)

	renderer.Present()
}

func blitText(font *sdlttf.Font, fg sdl.Color, x, y int, text string) {
	r, err := font.RenderUTF8BlendedEx(surface, text, fg)
	ck(err)
	draw.Draw(canvas, image.Rect(x, y, x+int(r.W), y+int(r.H)), surface, image.ZP, draw.Over)
}

func playMusic(mus *sdlmixer.Music) {
	if !conf.music || mus == nil {
		return
	}
	mus.Play(-1)
}

func playSound(snd *sdlmixer.Chunk, channel int) {
	if !conf.sound || snd == nil {
		return
	}
	snd.PlayChannel(channel, 0)
}

func bounds(e *Entity) image.Rectangle {
	x := int(e.X)
	y := int(e.Y)
	return image.Rect(x, y, x+int(e.Size.X), y+int(e.Size.Y))
}

func collide(p, q *Entity) bool {
	r1 := bounds(p)
	r2 := bounds(q)

	r := r1.Intersect(r2)
	if r.Empty() {
		return false
	}

	x1 := r.Min.X - r1.Min.X
	y1 := r.Min.Y - r1.Min.Y
	x2 := r.Min.X - r2.Min.X
	y2 := r.Min.Y - r2.Min.Y

	for y := 0; y < r.Dy(); y++ {
		for x := 0; x < r.Dx(); x++ {
			a := p.RGBAAt(x+x1, y+y1)
			b := q.RGBAAt(x+x2, y+y2)
			if a.A&b.A != 0 {
				return true
			}
		}
	}

	return false
}

func ck(err error) {
	if err != nil {
		sdl.LogCritical(sdl.LOG_CATEGORY_APPLICATION, "%v", err)
		sdl.ShowSimpleMessageBox(sdl.MESSAGEBOX_ERROR, "Error", err.Error(), window)
		os.Exit(1)
	}
}

func ek(err error) bool {
	if err != nil {
		sdl.LogError(sdl.LOG_CATEGORY_APPLICATION, "%v", err)
		return true
	}
	return false
}

type Image struct {
	*image.RGBA
	W, H int
}

func newImageUniform(color color.Color, size ga.Vec2d) *Image {
	w, h := int(size.X), int(size.Y)
	m := image.NewRGBA(image.Rect(0, 0, w, h))
	draw.Draw(m, m.Bounds(), image.NewUniform(color), image.ZP, draw.Src)
	return &Image{
		RGBA: m,
		W:    w,
		H:    h,
	}
}

func (m *Image) Blit(x, y int, op draw.Op) {
	draw.Draw(canvas, image.Rect(x, y, x+m.W, y+m.H), m.RGBA, image.ZP, op)
}

type Entity struct {
	*Image
	ga.Vec2d
	Size       ga.Vec2d
	Vector     ga.Vec2d
	Travel     ga.Vec2d
	Speed      uint32
	Time       uint32
	PauseDelta uint32
	Dead       bool
	Exploding  bool
	Explosion  *Image
	ExpAnimPos int
	ExpPos     ga.Vec2d
	ExpSize    ga.Vec2d
}

func (e *Entity) Blit() {
	e.Image.Blit(int(e.X), int(e.Y), draw.Over)
}

func (e *Entity) Pause() {
	e.PauseDelta = gtime - e.Time
}

func (e *Entity) Unpause() {
	e.Time = gtime - e.PauseDelta
}

type Player struct {
	Entity
	Score   uint64
	Round   uint32
	Lives   int
	Anim    int
	AnimPos int
	Shoot   bool
}

func newPlayer() *Player {
	size := ga.Vec2d{64, 61}
	return &Player{
		Entity: Entity{
			Image:     loadImage("graphics/ship_sheet_final.png", color.Black),
			Vec2d:     ga.Vec2d{(Res.X - size.X) / 2, 520},
			Size:      size,
			Travel:    ga.Vec2d{7, 0},
			Speed:     350,
			Time:      sdl.GetTicks(),
			Explosion: loadImage("graphics/explosion_new1.png", color.Transparent),
			ExpSize:   ga.Vec2d{79, 96},
		},
		Lives:   3,
		AnimPos: 5,
		Round:   1,
	}
}

func (p *Player) Frag() {
	const MaxScore = 1e9
	if p.Score < MaxScore {
		p.Score += 10
	}
}

func (p *Player) Update() {
	if p.Exploding {
		if p.ExpAnimPos < 8 {
			p.ExpAnimPos++
		} else {
			p.Exploding = false
			p.ExpAnimPos = 0
			if p.Lives--; p.Lives <= 0 {
				p.Dead = true
			}
		}
		return
	}

	p.animate()

	p.X += p.Vector.X * p.Travel.X
	if p.X < 0 {
		p.X = 0
	} else if p.X > Res.X-p.Size.X {
		p.X = Res.X - p.Size.X
	}
}

func (p *Player) animate() {
	if p.Anim&0x1 != 0 {
		if p.AnimPos < 10 {
			p.AnimPos++
		}
	} else {
		if p.AnimPos > 5 {
			p.AnimPos--
		}
	}

	if p.Anim&0x2 != 0 {
		if p.AnimPos > 0 {
			p.AnimPos--
		}
	} else {
		if p.AnimPos < 5 {
			p.AnimPos++
		}
	}
}

func (p *Player) Blit() {
	if !p.Exploding {
		x, y := int(p.X), int(p.Y)
		w, h := int(p.Size.X), int(p.Size.Y)
		draw.Draw(canvas, image.Rect(x, y, x+w, y+h), p.RGBA, image.Pt(p.AnimPos*w, 0), draw.Over)
		return
	}

	x, y := int(p.X-10), int(p.Y-30)
	w, h := int(p.ExpSize.X), int(p.ExpSize.Y)
	draw.Draw(canvas, image.Rect(x, y, x+w, y+h), p.Explosion.RGBA, image.Pt(0, p.ExpAnimPos*h), draw.Over)
}

type Alien struct {
	Entity
	Move ga.Vec2d
}

func newAlien(col, row int, speed uint32) *Alien {
	if speed < 700 {
		speed = 700 - speed
	} else {
		speed = 0
	}

	return &Alien{
		Entity: Entity{
			Image: loadImage("graphics/Spaceship16.png", color.White),
			Vec2d: ga.Vec2d{
				X: float64(AlienSpacer + row*(int(AlienSize.X)+AlienSpacer)),
				Y: float64(65 + col*(int(AlienSize.Y)+AlienSpacer)),
			},
			Size:      AlienSize,
			Vector:    ga.Vec2d{1, 1},
			Travel:    ga.Vec2d{AlienSize.X - 7, AlienSpacer},
			Speed:     speed,
			Time:      sdl.GetTicks(),
			Explosion: loadImage("graphics/alien_explosion.png", color.Transparent),
			ExpSize:   ga.Vec2d{94, 96},
		},
	}
}

func (a *Alien) Update() {
	if a.Exploding {
		if a.ExpAnimPos < 9 {
			a.ExpAnimPos++
		} else {
			a.Exploding = false
			a.Dead = true
		}
		return
	}
	if gtime-a.Time > a.Speed {
		if a.Move.X < 12 {
			a.X += a.Vector.X * a.Travel.X
			a.Move.X++
		} else {
			if a.Move.Y == 0 {
				a.Y += a.Vector.Y * a.Travel.Y
			}
			a.Vector.X *= -1
			a.Move = ga.Vec2d{}
			if a.Speed -= 20; a.Speed < 100 {
				a.Speed = 100
			}
		}
		a.Time = gtime
	}
}

func (a *Alien) Blit() {
	if a.Dead {
		return
	}
	if !a.Exploding {
		a.Entity.Blit()
		return
	}

	x, y := int(a.ExpPos.X-50), int(a.ExpPos.Y-60)
	w, h := int(a.ExpSize.X), int(a.ExpSize.Y)
	draw.Draw(canvas, image.Rect(x, y, x+w, y+h), a.Explosion.RGBA, image.Pt(0, a.ExpAnimPos*h), draw.Over)
}

func makeAliens(speed uint32) []*Alien {
	var aliens []*Alien
	for y := 0; y < BarrierRow; y++ {
		for x := 0; x < BarrierColumn; x++ {
			aliens = append(aliens, newAlien(x, y, speed))
		}
	}
	return aliens
}

type Ammo struct {
	Entity
}

func newAmmo(color color.Color, size ga.Vec2d) *Ammo {
	return &Ammo{
		Entity: Entity{
			Image: newImageUniform(color, size),
			Size:  size,
		},
	}
}

func (a *Ammo) Update() {
	a.Y += a.Vector.Y * float64(a.Speed)
	if a.Y < 0 || a.Y > Res.Y {
		a.Dead = true
	}
}

func makeBullet() {
	if gtime-player.Time > player.Speed {
		b := newAmmo(sdlcolor.Blue, BulletSize)
		b.Vector = ga.Vec2d{0, -1}
		b.Speed = 20
		b.X = player.X + 29
		b.Y = player.Y - 6
		bullets = append(bullets, b)
		player.Time = gtime
	}
	player.Shoot = false
	playSound(sfx.bullet, 0)
}

func makeMissile() {
	if len(aliens) == 0 || rand.Float64() > 0.05 {
		return
	}
	n := rand.Intn(len(aliens))
	a := aliens[n]
	m := newAmmo(sdlcolor.Red, MissileSize)
	m.Vector = ga.Vec2d{1, 1}
	m.X = a.X + 15
	m.Y = a.Y + 40
	m.Speed = 10
	missiles = append(missiles, m)
	playSound(sfx.bullet, 0)
}

type Barrier struct {
	Entity
}

func newBarrier(color color.Color, col, row, spacing int) *Barrier {
	x := 55 + 200*spacing + row*10
	y := 450 + col*10
	return &Barrier{
		Entity: Entity{
			Image: newImageUniform(color, BlockSize),
			Vec2d: ga.Vec2d{float64(x), float64(y)},
			Size:  BlockSize,
		},
	}
}

func makeDefenses() []*Barrier {
	var barriers []*Barrier
	for i := 0; i < 4; i++ {
		for j := 0; j < 3; j++ {
			for k := 0; k < 9; k++ {
				barriers = append(barriers, newBarrier(color.White, j, k, i))
			}
		}
	}
	return barriers
}
