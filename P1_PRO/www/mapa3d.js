/* =========================================================================
   mapa3d.js · Mapas 3D (MapLibre GL + deck.gl) para el prototipo P1
   R se comunica con este archivo por mensajes personalizados de Shiny:
     p1_static  · datos fijos (distritos, sitios, servicios, costa)
     p1_update  · estado de la simulación (intensidad, alertas, estado de sitios)
     p1_animate · animación de las ondas P y S
     p1_quakes  · sismos (catálogo histórico y monitoreo en vivo)
     p1_fly     · mover la cámara
   Y este archivo devuelve los clics del mapa a R: input$<id>_click
   ========================================================================= */
(function () {
  const MAPS = {};
  window.__P1MAPS = MAPS;   // para depuración desde la consola
  let MAPS_L = null;
  const BASE_STYLE = 'https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json';
  const DEM = 'https://s3.amazonaws.com/elevation-tiles-prod/terrarium/{z}/{x}/{y}.png';
  const SAT = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}';
  const OFM = 'https://tiles.openfreemap.org/planet';          // edificios OSM (OpenMapTiles), sin clave
  const EXAG = 1.6;                                             // exageración vertical del relieve

  const MMI_COLORS = [
    [255,255,255],[191,204,255],[160,230,255],[128,255,255],[122,255,147],
    [255,255,0],[255,200,0],[255,145,0],[255,0,0],[200,0,0]];
  function mmiColor(m, a) {
    const c = MMI_COLORS[Math.max(0, Math.min(9, Math.round(m) - 1))];
    return [c[0], c[1], c[2], a];
  }
  // Interpolación continua entre grados (como el ShakeMap del USGS)
  function mmiSuave(m) {
    const x = Math.max(1, Math.min(10, m)) - 1, i = Math.min(8, Math.floor(x)), f = x - i;
    const a = MMI_COLORS[i], b = MMI_COLORS[i + 1];
    return [a[0] + (b[0] - a[0]) * f, a[1] + (b[1] - a[1]) * f, a[2] + (b[2] - a[2]) * f];
  }
  function avisoSuave(s) {
    if (s <= 0) return [213, 94, 0];
    if (s < 10) { const f = s / 10; return [213 + 27 * f, 94 + 134 * f, 66 * f]; }
    const f = Math.min(1, (s - 10) / 20);
    return [240 - 240 * f, 228 - 70 * f, 66 + 49 * f];
  }
  function avisoColor(s, a) {
    if (s === null || s === undefined || isNaN(s)) return [120,120,120,a];
    if (s <= 0) return [213,94,0,a];
    if (s < 10) return [240,228,66,a];
    return [0,158,115,a];
  }
  const EST_COLOR = [[0,158,115],[240,228,66],[213,94,0],[86,180,233]];  // Okabe-Ito (apta para daltonismo)
  const EST_ELEV = [230, 340, 900, 300];   // altura de la columna (m): las caídas resaltan
  function depthColor(d) {
    if (d < 40) return [255,70,70];
    if (d < 70) return [255,160,40];
    if (d < 150) return [255,230,80];
    if (d < 300) return [80,200,120];
    return [70,130,255];
  }

  function crearMapa(el) {
    const id = el.id;
    const center = JSON.parse(el.dataset.center || '[-77.03,-12.06]');
    const map = new maplibregl.Map({
      container: el, style: BASE_STYLE, center: center,
      zoom: parseFloat(el.dataset.zoom || '9.6'), pitch: 58, bearing: -18,
      maxPitch: 85, attributionControl: { compact: true }
    });
    map.addControl(new maplibregl.NavigationControl({ visualizePitch: true }), 'top-right');
    // MapLibre 4.7: si una petición cancelada (AbortError) ocurre dentro de una tarea de render,
    // la cola queda marcada como «en ejecución» y el mapa se congela. Se libera y se ignora ese caso.
    const cola = map._renderTaskQueue;
    if (cola && cola.run) {
      const correr = cola.run.bind(cola);
      cola.run = function (t) {
        try { return correr(t); }
        catch (e) {
          cola._currentlyRunning = false;
          if (e && (e.name === 'AbortError' || /aborted/i.test(e.message || ''))) { map.triggerRepaint(); return; }
          throw e;
        }
      };
    }
    const st = { id, map, overlay: null, ready: false, queue: [], data: {}, upd: null,
                 anim: null, quakes: null, live: null, pulse: 0, terrain: false };
    const overlay = new deck.MapboxOverlay({
      interleaved: false, layers: [],
      getTooltip: info => { const t = tooltip(info); st.deckHover = !!t; return t; }
    });
    map.addControl(overlay);
    st.overlay = overlay;
    MAPS[id] = st;

    map.on('load', () => {
      const sym = (map.getStyle().layers.find(l => l.type === 'symbol') || {}).id;
      map.addSource('dem', { type: 'raster-dem', tiles: [DEM], encoding: 'terrarium', tileSize: 256, maxzoom: 13 });
      map.addSource('dem-hs', { type: 'raster-dem', tiles: [DEM], encoding: 'terrarium', tileSize: 256, maxzoom: 13 });
      // Imagen satelital (Esri World Imagery)
      map.addSource('sat', { type: 'raster', tiles: [SAT], tileSize: 256, maxzoom: 19,
        attribution: 'Imágenes © Esri, Maxar, Earthstar Geographics' });
      map.addLayer({ id: 'sat', type: 'raster', source: 'sat', layout: { visibility: 'none' },
        paint: { 'raster-saturation': -0.15, 'raster-brightness-max': 0.85 } }, sym);
      map.addLayer({ id: 'hillshade', type: 'hillshade', source: 'dem-hs',
        paint: { 'hillshade-exaggeration': 0.6, 'hillshade-shadow-color': 'rgba(0,0,0,0.75)',
                 'hillshade-highlight-color': 'rgba(255,255,255,0.18)', 'hillshade-accent-color': 'rgba(0,0,0,0.35)',
                 'hillshade-illumination-direction': 315 } }, sym);
      st.sym = sym;
      // Distritos: contorno fino + relleno invisible para consultar con el cursor
      map.addSource('dist', { type: 'geojson', data: { type: 'FeatureCollection', features: [] } });
      map.addLayer({ id: 'dist-fill', type: 'fill', source: 'dist', paint: { 'fill-color': '#000', 'fill-opacity': 0.01 } }, sym);
      map.addLayer({ id: 'dist-line', type: 'line', source: 'dist',
        paint: { 'line-color': '#e6edf6', 'line-opacity': 0.45,
                 'line-width': ['interpolate', ['linear'], ['zoom'], 9, 0.5, 13, 1.6] } }, sym);
      map.addSource('costa', { type: 'geojson', data: { type: 'FeatureCollection', features: [] } });
      map.addLayer({ id: 'costa', type: 'line', source: 'costa',
        paint: { 'line-color': '#38bdf8', 'line-width': 1.2, 'line-opacity': 0.8 } }, sym);
      // Edificios reales de OpenStreetMap en 3D (desde zoom 12.5)
      try {
        map.addSource('ofm', { type: 'vector', url: OFM, attribution: '© OpenMapTiles © OpenStreetMap' });
        map.addLayer({ id: 'edificios', type: 'fill-extrusion', source: 'ofm', 'source-layer': 'building', minzoom: 12.5,
          paint: {
            'fill-extrusion-color': ['interpolate', ['linear'], ['coalesce', ['get', 'render_height'], 6],
              0, '#cbd5e1', 20, '#e2e8f0', 60, '#f8fafc'],
            'fill-extrusion-height': ['interpolate', ['linear'], ['zoom'], 12.5, 0, 13.5, ['coalesce', ['get', 'render_height'], 6]],
            'fill-extrusion-base': ['coalesce', ['get', 'render_min_height'], 0],
            'fill-extrusion-opacity': 0.82 } }, sym);
      } catch (e) { console.warn('Edificios OSM no disponibles', e); }
      // Ventana nativa: intensidad del punto bajo el cursor + datos del distrito
      st.popup = new maplibregl.Popup({ closeButton: false, closeOnClick: false, className: 'p1-pop', maxWidth: '300px', offset: 12 });
      map.on('mousemove', e => {
        if (st.deckHover) { st.popup.remove(); return; }
        const f = map.queryRenderedFeatures(e.point, { layers: ['dist-fill'] })[0];
        if (!f) { st.popup.remove(); map.getCanvas().style.cursor = ''; return; }
        map.getCanvas().style.cursor = 'crosshair';
        st.popup.setLngLat(e.lngLat).setHTML(textoPunto(st, f.properties, e.lngLat)).addTo(map);
      });
      map.on('mouseout', () => st.popup.remove());
      // La cola se vacía fuera del ciclo de render (evita «already running» de MapLibre)
      setTimeout(() => { st.ready = true; st.queue.forEach(fn => fn()); st.queue = []; }, 0);
    });
    let zt = null;
    map.on('zoom', () => { if (zt) return; zt = setTimeout(() => { zt = null; if (st.ready) render(st); }, 120); });
    map.on('click', e => {
      if (window.Shiny) Shiny.setInputValue(id + '_click',
        { lat: e.lngLat.lat, lon: e.lngLat.lng, n: Math.random() }, { priority: 'event' });
    });
    return st;
  }

  // Valor de la malla en un punto
  function valorMalla(g, arr, lng, lat) {
    if (!g) return null;
    const W = g.bbox[0], E = g.bbox[1], S = g.bbox[2], N = g.bbox[3];
    const c = Math.floor((lng - W) / (E - W) * g.w), r = Math.floor((N - lat) / (N - S) * g.h);
    if (c < 0 || r < 0 || c >= g.w || r >= g.h) return null;
    return arr[r * g.w + c];
  }
  const ROM = ['I','II','III','IV','V','VI','VII','VIII','IX','X'];
  const DESC = ['No sentido','Débil','Leve','Ligero','Moderado','Fuerte','Muy fuerte','Severo','Violento','Extremo'];
  function textoPunto(st, p, ll) {
    let h = `<b>${p.distrito}</b><br><span class="pp-sub">Población (Censo 2025): ${Number(p.poblacion).toLocaleString('es-PE')}</span>`;
    const g = st.malla;
    const m = g ? valorMalla(g, g.mmi, ll.lng, ll.lat) : null;
    if (m !== null && m >= 1) {
      const k = Math.max(0, Math.min(9, Math.round(m) - 1)), c = mmiSuave(m).map(Math.round).join(',');
      const pga = Math.pow(10, (m + 1.66) / 3.66) / 981;
      h += `<div class="pp-mmi" style="border-color:rgb(${c})"><span style="background:rgb(${c})">${ROM[k]}</span>` +
           `<div>Intensidad en este punto: <b>${m.toFixed(1)}</b> · ${DESC[k]}<br>Aceleración ≈ ${pga.toFixed(2)} g</div></div>`;
      const a = valorMalla(g, g.aviso, ll.lng, ll.lat);
      if (a !== null && a > -9999) h += `Aviso sísmico: <b>${a > 0 ? a + ' s antes' : Math.abs(a) + ' s DESPUÉS'}</b> de la onda S`;
      if (p._tsu && p._tsu !== 'null') h += `<br>🌊 Tsunami: arribo en ≈ ${p._tsu} min`;
      h += `<br><span class="pp-sub">Promedio del distrito: MMI ${p._mmiR} · suelo ${p.suelo || ''}</span>`;
    } else if (p.suelo) h += `<br><span class="pp-sub">Suelo: ${p.suelo}</span>`;
    return h;
  }
  // Dibuja la malla como imagen y la pega al relieve
  function pintarMalla(st) {
    const g = st.malla, map = st.map, U = st.upd || {};
    if (!g) return;
    // Fuente «canvas»: sin peticiones de red, nada que cancelar (updateImage dejaba trabado el render)
    if (!st.cv || st.cv.width !== g.w || st.cv.height !== g.h) {
      st.cv = document.createElement('canvas'); st.cv.width = g.w; st.cv.height = g.h;
    }
    const ctx = st.cv.getContext('2d'), img = ctx.createImageData(g.w, g.h), modo = U.modo;
    for (let i = 0; i < g.w * g.h; i++) {
      const v = g.mmi[i];
      if (v < 1) { img.data[i * 4 + 3] = 0; continue; }
      const c = modo === 'aviso' ? avisoSuave(g.aviso[i]) : mmiSuave(v);
      img.data[i * 4] = c[0]; img.data[i * 4 + 1] = c[1]; img.data[i * 4 + 2] = c[2]; img.data[i * 4 + 3] = 255;
    }
    ctx.putImageData(img, 0, 0);
    const W = g.bbox[0], E = g.bbox[1], S = g.bbox[2], N = g.bbox[3];
    const coords = [[W, N], [E, N], [E, S], [W, S]];
    const src = map.getSource('mmi');
    if (src && src.canvas === st.cv) {
      src.setCoordinates(coords);
      src.play(); setTimeout(() => src.pause(), 120);     // vuelve a leer los píxeles del canvas
    } else {
      if (map.getLayer('mmi')) map.removeLayer('mmi');
      if (src) map.removeSource('mmi');
      map.addSource('mmi', { type: 'canvas', canvas: st.cv, coordinates: coords, animate: false });
      map.addLayer({ id: 'mmi', type: 'raster', source: 'mmi',
        paint: { 'raster-opacity': 0.62, 'raster-resampling': 'linear', 'raster-fade-duration': 0 } }, 'hillshade');
    }
    aplicarVisibilidad(st);
  }
  function aplicarVisibilidad(st) {
    const map = st.map, U = st.upd || {}, vis = U.vis || {};
    const set = (id, on) => { if (map.getLayer(id)) map.setLayoutProperty(id, 'visibility', on ? 'visible' : 'none'); };
    const sat = U.base === 'satelite';
    set('sat', sat);
    if (map.getLayer('hillshade')) map.setPaintProperty('hillshade', 'hillshade-exaggeration', sat ? 0.25 : 0.6);
    if (map.getLayer('mmi')) map.setPaintProperty('mmi', 'raster-opacity',
      ['interpolate', ['linear'], ['zoom'], 10, sat ? 0.5 : 0.68, 14, sat ? 0.22 : 0.4]);
    set('mmi', vis.distritos !== false); set('dist-line', vis.distritos !== false);
    set('costa', vis.costa !== false); set('edificios', vis.edificios !== false);
  }

  // A escala de ciudad las columnas son grandes para verse; de cerca, alturas realistas de torre (20-70 m)
  function escalaZoom(st) {
    const z = st.map.getZoom();
    return Math.max(0.08, Math.min(1, Math.pow(2, 10.6 - z)));
  }

  function cuando(id, fn) {
    const st = MAPS[id];
    if (!st) return;
    if (st.ready) fn(st); else st.queue.push(() => fn(st));
  }

  function tooltip(info) {
    const o = info.object;
    if (!o && info.index < 0) return null;
    const L = info.layer && info.layer.id;
    const box = h => ({ html: h, style: { background: 'rgba(15,20,30,.92)', color: '#e8eef5',
      fontSize: '12px', borderRadius: '6px', padding: '8px 10px', maxWidth: '280px' } });
    if (L === 'distritos' && o) {
      const p = o.properties;
      let h = `<b>${p.distrito}</b><br>Población (Censo 2025): ${Number(p.poblacion).toLocaleString('es-PE')}`;
      if (p._mmi !== undefined) {
        h += `<br>Intensidad: <b>MMI ${p._mmiR}</b> (PGA ≈ ${p._pga} g)`;
        h += `<br>Aviso sísmico: <b>${p._aviso > 0 ? p._aviso + ' s antes' : Math.abs(p._aviso) + ' s DESPUÉS'}</b> de la onda S`;
        if (p._tsu) h += `<br>🌊 Tsunami: arribo en ≈ ${p._tsu} min`;
      }
      return box(h);
    }
    if (L === 'sitios') {
      const st = info.layer.props.meta;
      const i = info.index;
      const e = ['Operativo','Con batería (sin red eléctrica)','CAÍDO','Restablecido'][st.est[i]];
      return box(`<b>Sitio ${st.ids[i]}</b> · Operador ${st.op[i]}<br>Estado: <b>${e}</b><br>${st.tec ? st.tec[i] + ' · ' + st.cel[i] + ' celda(s)<br>' : ''}Fuente: ${st.fuente[i]} (CC BY-SA 4.0)`);
    }
    if (L === 'servicios' && o) return box(`<b>${o.nombre || 'Sin nombre'}</b><br>${o.amenity === 'hospital' ? 'Hospital' : 'Estación de bomberos'} (OSM)`);
    if (L === 'ruptura') return box(`<b>Ruptura modelada</b><br>Longitud ≈ ${(MAPS_L||'')} km (Wells & Coppersmith 1994), azimut 325°`);
    if ((L === 'historicos' || L === 'vivo' || L === 'vivo-halo') && o)
      return box(`<b>M${o.mag}</b> · ${o.fecha}<br>${o.lugar}<br>Profundidad: ${o.prof} km`);
    return null;
  }

  // ----------------------------------------------------------------- capas
  function render(st) {
    const layers = [];
    const D = st.data, U = st.upd || {}, vis = U.vis || {};
    if (D.sitios && vis.sitios !== false) {
      const S = D.sitios, est = U.est || new Array(S.lon.length).fill(0), ops = U.ops || ['A','B','C','D'];
      const meta = { est, ids: S.id, op: S.op, fuente: S.fuente, tec: S.tec, cel: S.cel };
      layers.push(new deck.ColumnLayer({
        id: 'sitios', data: { length: S.lon.length }, meta, pickable: true,
        diskResolution: 8, radius: 55 * escalaZoom(st), extruded: true, elevationScale: escalaZoom(st),
        material: { ambient: 0.45, diffuse: 0.65, shininess: 40, specularColor: [255, 255, 255] },
        getPosition: (_, { index }) => [S.lon[index], S.lat[index], st.terrain && S.z ? S.z[index] * EXAG : 0],
        getElevation: (_, { index }) => ops.includes(S.op[index]) ? EST_ELEV[est[index]] : 0,
        getFillColor: (_, { index }) => ops.includes(S.op[index]) ? EST_COLOR[est[index]].concat([230]) : [0,0,0,0],
        updateTriggers: { getPosition: [st.terrain], getElevation: [U.version, U.tver, ops.join()], getFillColor: [U.version, U.tver, ops.join()] }
      }));
    }
    if (D.servicios && vis.servicios !== false) {
      layers.push(new deck.ScatterplotLayer({
        id: 'servicios', data: D.servicios, pickable: true, radiusUnits: 'pixels',
        getPosition: d => [d.lon, d.lat, st.terrain && d.z ? d.z * EXAG + 40 : 0], getRadius: 6, stroked: true, lineWidthMinPixels: 2,
        updateTriggers: { getPosition: [st.terrain] },
        getFillColor: d => d.amenity === 'hospital' ? [255,255,255,240] : [255,120,0,240],
        getLineColor: d => d.amenity === 'hospital' ? [220,20,60,255] : [80,30,0,255]
      }));
    }
    if (U.ruptura && U.ruptura.length === 2) {
      layers.push(new deck.PathLayer({ id: 'ruptura', data: [{ path: U.ruptura }], pickable: true,
        getPath: d => d.path, getColor: [255, 0, 128, 230], widthUnits: 'pixels', getWidth: 6,
        capRounded: true, jointRounded: true }));
    }
    if (U.epi) {
      layers.push(new deck.ScatterplotLayer({ id: 'epi', data: [U.epi], radiusUnits: 'pixels',
        getPosition: d => [d.lon, d.lat], getRadius: 9 + 4 * Math.sin(st.pulse),
        getFillColor: [255,0,80,255], stroked: true, getLineColor: [255,255,255], lineWidthMinPixels: 2,
        updateTriggers: { getRadius: [st.pulse] } }));
      if (U.tsunami && U.tsuKm > 0) {
        layers.push(new deck.ScatterplotLayer({ id: 'tsunami', data: [U.epi], stroked: true, filled: true,
          getPosition: d => [d.lon, d.lat], getRadius: U.tsuKm * 1000,
          getFillColor: [0,220,255,25], getLineColor: [0,240,255,230], lineWidthMinPixels: 3 }));
      }
    }
    if (st.anim) {
      const a = st.anim;
      layers.push(new deck.ScatterplotLayer({ id: 'onda-p', data: [a.epi], stroked: true, filled: false,
        getPosition: d => [d.lon, d.lat], getRadius: a.rp * 1000, getLineColor: [80,170,255,220], lineWidthMinPixels: 2,
        updateTriggers: { getRadius: [a.t] } }));
      layers.push(new deck.ScatterplotLayer({ id: 'onda-s', data: [a.epi], stroked: true, filled: true,
        getPosition: d => [d.lon, d.lat], getRadius: a.rs * 1000, getFillColor: [255,60,60,30],
        getLineColor: [255,60,60,240], lineWidthMinPixels: 4, updateTriggers: { getRadius: [a.t] } }));
    }
    if (st.quakes && (st.qvis !== false)) {
      const Q = st.quakes;
      layers.push(new deck.ColumnLayer({
        id: 'historicos', data: Q, pickable: true, diskResolution: 12, radius: 6500, extruded: true,
        getPosition: d => [d.lon, d.lat], getElevation: d => Math.pow(Math.max(0, d.mag - 4), 2.2) * 9000,
        getFillColor: d => depthColor(d.prof).concat([200])
      }));
    }
    if (st.live) {
      const k = 1 + 0.35 * Math.sin(st.pulse * 1.5);
      layers.push(new deck.ScatterplotLayer({ id: 'vivo-halo', data: st.live, pickable: true,
        getPosition: d => [d.lon, d.lat], radiusUnits: 'meters',
        getRadius: d => Math.pow(2, d.mag) * 900 * k, stroked: true, filled: true,
        getFillColor: d => d.recent ? [255,40,80,60] : [255,200,0,35],
        getLineColor: d => d.recent ? [255,40,80,255] : [255,200,0,200], lineWidthMinPixels: 2,
        updateTriggers: { getRadius: [st.pulse] } }));
      layers.push(new deck.ScatterplotLayer({ id: 'vivo', data: st.live, pickable: true,
        getPosition: d => [d.lon, d.lat], radiusUnits: 'pixels', getRadius: 4,
        getFillColor: [255,255,255,255] }));
    }
    st.overlay.setProps({ layers });
  }

  // Pulso continuo (epicentro y sismos en vivo)
  function latido() {
    if (!document.hidden) {
      Object.values(MAPS).forEach(st => {
        const visible = st.map.getContainer().offsetParent !== null;
        if (visible && ((st.upd && st.upd.epi) || st.live)) { st.pulse += 0.2; if (!st.anim) render(st); }
      });
    }
    setTimeout(() => requestAnimationFrame(latido), 150);
  }

  // ----------------------------------------------------------------- mensajes de Shiny
  function registrar() {
    Shiny.addCustomMessageHandler('p1_static', msg => cuando(msg.id, st => {
      st.data.distritos = msg.distritos; st.data.costa = msg.costa;
      if (msg.distritos && st.map.getSource('dist')) st.map.getSource('dist').setData(msg.distritos);
      if (msg.costa && st.map.getSource('costa')) st.map.getSource('costa').setData(msg.costa);
      st.data.sitios = msg.sitios; st.data.servicios = typeof msg.servicios === 'string' ? JSON.parse(msg.servicios) : msg.servicios;
      render(st);
    }));
    Shiny.addCustomMessageHandler('p1_update', msg => cuando(msg.id, st => {
      const prev = st.upd || {};
      st.upd = Object.assign({}, prev, msg);
      if (msg.L_km !== undefined) MAPS_L = msg.L_km;
      if (msg.mmi && st.data.distritos) {
        st.data.distritos.features.forEach((f, i) => {
          f.properties._mmi = msg.mmi[i]; f.properties._mmiR = msg.mmiR[i];
          f.properties._pga = msg.pga[i]; f.properties._aviso = msg.aviso[i];
          f.properties._tsu = msg.tsu ? msg.tsu[i] : null;
        });
        if (st.map.getSource('dist')) st.map.getSource('dist').setData(st.data.distritos);
      }
      if (msg.malla) st.malla = msg.malla;
      if (msg.malla || (msg.modo !== undefined && msg.modo !== prev.modo)) pintarMalla(st);
      if (msg.terrain !== undefined && msg.terrain !== st.terrain) {
        st.terrain = msg.terrain;
        st.map.setTerrain(msg.terrain ? { source: 'dem', exaggeration: EXAG } : null);
      }
      aplicarVisibilidad(st);
      render(st);
    }));
    Shiny.addCustomMessageHandler('p1_animate', msg => cuando(msg.id, st => {
      const t0 = performance.now(), dur = msg.dur_ms || 14000, simMax = msg.sim_s || 90;
      const lbl = document.getElementById(msg.id + '_reloj');
      let sono = false;
      if (window.SismoSonido) window.SismoSonido.play('sismo');     // retumbo al iniciar el sismo
      function paso(now) {
        const f = Math.min(1, (now - t0) / dur), t = f * simMax;
        st.anim = { epi: msg.epi, t, rp: Math.sqrt(Math.max(0, Math.pow(6.5 * t, 2) - Math.pow(msg.prof, 2))),
                    rs: Math.sqrt(Math.max(0, Math.pow(3.5 * t, 2) - Math.pow(msg.prof, 2))) };
        if (lbl) {
          const enviada = t >= msg.t_alerta;
          if (enviada && !sono) { sono = true; if (window.SismoSonido) window.SismoSonido.play('sismate'); }
          lbl.innerHTML = `⏱ t = ${t.toFixed(0)} s &nbsp;·&nbsp; <span style="color:#5aaaff">onda P</span> ${st.anim.rp.toFixed(0)} km &nbsp;·&nbsp; <span style="color:#ff5c5c">onda S</span> ${st.anim.rs.toFixed(0)} km &nbsp;·&nbsp; ` +
            (enviada ? '<b style="color:#2ecc71">📲 ALERTA ENVIADA</b>' :
              `<b style="color:#f1c40f">alerta en ${(msg.t_alerta - t).toFixed(0)} s</b>`);
          lbl.style.display = 'block';
        }
        render(st);
        if (f < 1) requestAnimationFrame(paso);
        else { st.anim = null; render(st); if (lbl) setTimeout(() => { lbl.style.display = 'none'; }, 4000); }
      }
      requestAnimationFrame(paso);
    }));
    Shiny.addCustomMessageHandler('p1_quakes', msg => cuando(msg.id, st => {
      if (msg.historicos !== undefined) st.quakes = typeof msg.historicos === 'string' ? JSON.parse(msg.historicos) : msg.historicos;
      if (msg.vivo !== undefined) st.live = typeof msg.vivo === 'string' ? JSON.parse(msg.vivo) : msg.vivo;
      if (msg.qvis !== undefined) st.qvis = msg.qvis;
      if (msg.terrain !== undefined && msg.terrain !== st.terrain) {
        st.terrain = msg.terrain;
        st.map.setTerrain(msg.terrain ? { source: 'dem', exaggeration: EXAG } : null);
      }
      render(st);
    }));
    Shiny.addCustomMessageHandler('p1_fly', msg => cuando(msg.id, st => {
      st.map.flyTo({ center: [msg.lon, msg.lat], zoom: msg.zoom || st.map.getZoom(),
                     pitch: msg.pitch || 58, bearing: msg.bearing || -18, duration: 2500 });
    }));
  }

  function iniciar() {
    document.querySelectorAll('.p1map').forEach(el => { if (!MAPS[el.id]) crearMapa(el); });
    // Los mapas en pestañas ocultas necesitan resize al mostrarse
    document.addEventListener('shown.bs.tab', () =>
      setTimeout(() => Object.values(MAPS).forEach(st => st.map.resize()), 50));
    latido();
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) registrar();
  else document.addEventListener('DOMContentLoaded', registrar);
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', iniciar);
  else iniciar();
})();
