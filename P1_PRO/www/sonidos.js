/* =========================================================================
   SOS Net Perú · Alerta sísmica sonora (Web Audio API + voz del navegador)
   Diseño inspirado en los sistemas de alerta sísmica pública (sirena de
   defensa civil + voz «Alerta sísmica»), no en alarmas de auto:
     CRÍTICA  → campanas de atención + sirena de defensa civil (subida lenta,
                sostenida, bajada) + voz «¡Alerta sísmica! ¡Alerta sísmica!»
                y destello rojo en pantalla
     ALTA     → campanas de atención (dos veces) + sirena corta
     MEDIA    → una campana de atención
     sismate  → patrón oficial del SISMATE (RM 896-2021-MTC): 2 s + 1 s + 1 s a 960 Hz
     sismo    → retumbo grave del terremoto al iniciar la simulación
   Botón de silencio en la barra superior. El navegador solo permite sonido
   después de la primera interacción (un clic en la página).
   ========================================================================= */
(function () {
  let ctx = null, salida = null;
  let activo = true;
  try { activo = localStorage.getItem('sosnet_sonido') !== 'off'; } catch (e) {}
  const cola = [];
  let ocupado = false;

  // Reverberación de exterior (respuesta al impulso generada): la sirena «suena en la calle»
  function impulso(c, seg = 2.2) {
    const n = Math.floor(c.sampleRate * seg), b = c.createBuffer(2, n, c.sampleRate);
    for (let ch = 0; ch < 2; ch++) {
      const d = b.getChannelData(ch);
      for (let i = 0; i < n; i++) d[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / n, 3);
    }
    return b;
  }
  function audio() {
    if (!ctx) {
      const AC = window.AudioContext || window.webkitAudioContext;
      if (!AC) return null;
      ctx = new AC();
      const seco = ctx.createGain(); seco.gain.value = 0.8;
      const rev = ctx.createConvolver(); rev.buffer = impulso(ctx);
      const humedo = ctx.createGain(); humedo.gain.value = 0.35;
      const comp = ctx.createDynamicsCompressor();
      comp.threshold.value = -12; comp.ratio.value = 4;
      const vol = ctx.createGain(); vol.gain.value = 0.9;
      const entrada = ctx.createGain();
      entrada.connect(seco).connect(comp);
      entrada.connect(rev).connect(humedo).connect(comp);
      comp.connect(vol).connect(ctx.destination);
      salida = entrada;
    }
    if (ctx.state === 'suspended') ctx.resume();
    return ctx;
  }
  ['click', 'keydown', 'touchstart'].forEach(ev =>
    document.addEventListener(ev, () => audio(), { once: true, passive: true }));

  // Campana de atención (tono de campana con armónicos y decaimiento largo)
  function campana(c, t0, frec, vol = 0.22) {
    [[1, 1], [2.0, 0.45], [3.01, 0.25], [4.2, 0.12]].forEach(([m, a]) => {
      const o = c.createOscillator(), g = c.createGain();
      o.type = 'sine'; o.frequency.value = frec * m;
      g.gain.setValueAtTime(0.0001, t0);
      g.gain.exponentialRampToValueAtTime(vol * a, t0 + 0.01);
      g.gain.exponentialRampToValueAtTime(0.0001, t0 + 1.4);
      o.connect(g).connect(salida); o.start(t0); o.stop(t0 + 1.5);
    });
  }
  // Arpegio ascendente de atención (re - fa# - la - re')
  function atencion(c, t0) {
    [587.3, 740.0, 880.0, 1174.7].forEach((f, i) => campana(c, t0 + i * 0.16, f));
    return 0.16 * 4 + 0.6;
  }
  // Sirena de defensa civil: dos rotores (ligeramente desafinados), subida lenta,
  // meseta con leve ondulación y bajada lenta
  function sirena(c, t0, subida, meseta, bajada, fmax = 640, vol = 0.2) {
    const dur = subida + meseta + bajada;
    [[0, 'sawtooth', vol], [7, 'triangle', vol * 1.1]].forEach(([det, tipo, v]) => {
      const o = c.createOscillator(), f = c.createBiquadFilter(), g = c.createGain();
      o.type = tipo; o.detune.value = det;
      o.frequency.setValueAtTime(110, t0);
      o.frequency.exponentialRampToValueAtTime(fmax, t0 + subida);
      o.frequency.setValueAtTime(fmax, t0 + subida);
      o.frequency.linearRampToValueAtTime(fmax * 0.97, t0 + subida + meseta / 2);
      o.frequency.linearRampToValueAtTime(fmax, t0 + subida + meseta);
      o.frequency.exponentialRampToValueAtTime(140, t0 + dur);
      f.type = 'lowpass'; f.frequency.value = 2200; f.Q.value = 0.7;   // timbre redondo de bocina
      g.gain.setValueAtTime(0.0001, t0);
      g.gain.exponentialRampToValueAtTime(v, t0 + subida * 0.6);
      g.gain.setValueAtTime(v, t0 + subida + meseta);
      g.gain.exponentialRampToValueAtTime(0.0001, t0 + dur);
      o.connect(f).connect(g).connect(salida);
      o.start(t0); o.stop(t0 + dur + 0.05);
    });
    return dur;
  }
  // Voz sintética en español (si el navegador la tiene)
  function voz(texto) {
    try {
      if (!('speechSynthesis' in window)) return;
      const u = new SpeechSynthesisUtterance(texto);
      const vs = speechSynthesis.getVoices();
      u.voice = vs.find(v => /^es(-|_)(PE|419|MX|US)/i.test(v.lang)) || vs.find(v => /^es/i.test(v.lang)) || null;
      u.lang = (u.voice && u.voice.lang) || 'es-PE';
      u.rate = 0.95; u.pitch = 0.85; u.volume = 1;
      speechSynthesis.cancel(); speechSynthesis.speak(u);
    } catch (e) {}
  }
  function destello() {
    document.body.classList.remove('flash-rojo'); void document.body.offsetWidth;
    document.body.classList.add('flash-rojo');
    setTimeout(() => document.body.classList.remove('flash-rojo'), 2600);
  }

  const SONIDOS = {
    'CRÍTICA': c => {
      const t = c.currentTime;
      let d = atencion(c, t);
      d += atencion(c, t + d) - 0.2;
      setTimeout(() => voz('¡Alerta sísmica! ¡Alerta sísmica!'), d * 1000);
      const ds = sirena(c, t + d + 1.6, 1.8, 2.2, 2.4);
      destello();
      return d + 1.6 + ds;
    },
    'ALTA': c => {
      const t = c.currentTime;
      let d = atencion(c, t);
      d += atencion(c, t + d) - 0.2;
      return d + sirena(c, t + d, 1.0, 0.8, 1.2, 560, 0.14);
    },
    'MEDIA': c => atencion(c, c.currentTime),
    'sismate': c => {
      const t = c.currentTime;
      [[0, 2.0], [2.5, 1.0], [4.0, 1.0]].forEach(([d, dur]) => {
        [[960, 'sine', 0.24], [1920, 'sine', 0.06]].forEach(([f, tipo, v]) => {
          const o = c.createOscillator(), g = c.createGain();
          o.type = tipo; o.frequency.value = f;
          g.gain.setValueAtTime(0.0001, t + d);
          g.gain.exponentialRampToValueAtTime(v, t + d + 0.02);
          g.gain.setValueAtTime(v, t + d + dur - 0.04);
          g.gain.exponentialRampToValueAtTime(0.0001, t + d + dur);
          o.connect(g).connect(salida); o.start(t + d); o.stop(t + d + dur + 0.05);
        });
      });
      setTimeout(() => voz('Alerta de emergencia. Mensaje del SISMATE.'), 5300);
      return 7.5;
    },
    'sismo': c => {
      const t = c.currentTime, dur = 3.2, n = Math.floor(c.sampleRate * dur);
      const buf = c.createBuffer(1, n, c.sampleRate), d = buf.getChannelData(0);
      let ult = 0;
      for (let i = 0; i < n; i++) {
        const b = Math.random() * 2 - 1; ult = (ult + 0.02 * b) / 1.02;
        const crujido = Math.random() < 0.0004 ? (Math.random() * 2 - 1) * 0.8 : 0;
        d[i] = ult * 4.5 + crujido;
      }
      const src = c.createBufferSource(), fl = c.createBiquadFilter(), g = c.createGain();
      src.buffer = buf; fl.type = 'lowpass'; fl.frequency.value = 170;
      g.gain.setValueAtTime(0.0001, t);
      g.gain.exponentialRampToValueAtTime(1.5, t + 0.25);
      g.gain.setValueAtTime(1.5, t + 1.8);
      g.gain.exponentialRampToValueAtTime(0.0001, t + dur);
      src.connect(fl).connect(g).connect(salida); src.start(t);
      const o = c.createOscillator(), og = c.createGain();
      o.type = 'sine';
      [[0, 38], [0.8, 55], [1.6, 32], [2.4, 48], [dur, 30]].forEach(([x, f], i) =>
        i ? o.frequency.linearRampToValueAtTime(f, t + x) : o.frequency.setValueAtTime(f, t));
      og.gain.setValueAtTime(0.0001, t); og.gain.exponentialRampToValueAtTime(0.45, t + 0.3);
      og.gain.exponentialRampToValueAtTime(0.0001, t + dur);
      o.connect(og).connect(salida); o.start(t); o.stop(t + dur + 0.05);
      return dur;
    }
  };

  function siguiente() {
    if (!cola.length) { ocupado = false; return; }
    ocupado = true;
    const nivel = cola.shift();
    const c = audio();
    let dur = 0.5;
    try { if (c) dur = (SONIDOS[nivel] || SONIDOS['MEDIA'])(c); } catch (e) {}
    setTimeout(siguiente, (dur + 0.4) * 1000);
  }

  window.SismoSonido = {
    play(nivel) {
      if (!activo) return;
      if (cola.length > 2) return;
      cola.push(nivel);
      if (!ocupado) siguiente();
    },
    alternar() {
      activo = !activo;
      try { localStorage.setItem('sosnet_sonido', activo ? 'on' : 'off'); } catch (e) {}
      if (!activo) { cola.length = 0; try { speechSynthesis.cancel(); } catch (e) {} }
      actualizarBoton();
      if (activo) this.play('MEDIA');
    },
    activo: () => activo
  };
  if ('speechSynthesis' in window) speechSynthesis.onvoiceschanged = () => speechSynthesis.getVoices();

  function actualizarBoton() {
    const b = document.getElementById('btn_sonido');
    if (!b) return;
    b.innerHTML = activo ? '<i class="fas fa-volume-high"></i>' : '<i class="fas fa-volume-xmark"></i>';
    b.title = activo ? 'Alerta sísmica sonora activada (clic para silenciar)' : 'Alertas silenciadas (clic para activar)';
    b.classList.toggle('silenciado', !activo);
  }
  document.addEventListener('DOMContentLoaded', actualizarBoton);

  function registrar() {
    Shiny.addCustomMessageHandler('p1_sonido', msg => window.SismoSonido.play(msg.nivel));
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) registrar();
  else document.addEventListener('DOMContentLoaded', registrar);
})();
