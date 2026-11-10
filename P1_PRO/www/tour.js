/* Recorrido guiado (driver.js) · SOS Net Perú */
(function () {
  function irA(valor) {
    const a = document.querySelector(`a.nav-link[data-value="${valor}"]`);
    if (a) a.click();
  }
  function pasos() {
    return [
      { popover: { title: '👋 Bienvenido a SOS Net', description:
        'En 2 minutos verás cómo simular un terremoto en Lima, leer sus efectos sobre las telecomunicaciones y descubrir qué normas faltan.<br><br>Usa las flechas o los botones.' } },
      { element: '#galeria_casos', onHighlightStarted: () => irA('inicio'),
        popover: { title: '1 · Casos de ejemplo', description: 'La forma más rápida de empezar: cada tarjeta carga un sismo real o un escenario y lo simula con un clic.', side: 'top' } },
      { element: '#barra_sim', onHighlightStarted: () => irA('sim'),
        popover: { title: '2 · Configura el sismo', description: 'Paneles desplegables: <b>Sismo</b> (dónde y qué tan fuerte), <b>¿Qué pasaría si…?</b> (cerrar vacíos legales), <b>Supuestos</b> y <b>Capas del mapa</b>. El ícono <b>?</b> explica cada control.', side: 'right' } },
      { element: '#simular', popover: { title: '3 · Simula', description: 'Ejecuta la simulación: el mapa anima las ondas sísmicas y un reloj muestra cuándo llega la alerta.', side: 'top' } },
      { element: '#fila_kpi', popover: { title: '4 · Indicadores clave', description: 'Población afectada, antenas caídas, llamadas que fallan, alerta tardía y alertas activas. Debajo de cada número hay una interpretación breve.', side: 'bottom' } },
      { element: '#card_mapa', popover: { title: '5 · Mapa 3D', description: 'Distritos coloreados por intensidad, cada columna es una antena real (verde operativa, naranja caída). Pasa el cursor para ver detalles; haz clic para mover el epicentro.', side: 'right' } },
      { element: '#linea_tiempo', popover: { title: '6 · Línea de tiempo', description: 'Mueve el deslizador o pulsa ▶ para ver las 72 horas: baterías que se agotan, alertas que aparecen y antenas que se restablecen.', side: 'top' } },
      { element: '#card_lectura', popover: { title: '7 · Lectura del escenario', description: 'El sistema interpreta los resultados en lenguaje claro y sugiere qué política probar.', side: 'left' } },
      { element: '#btn_alertas', popover: { title: '8 · Centro de alertas', description: 'La campana reúne las alertas con su norma peruana, el vacío legal, la propuesta y el ejemplo internacional. También aparecen avisos emergentes al avanzar el tiempo.', side: 'bottom' } },
      { element: 'a.nav-link[data-value="agentes"]', popover: { title: '9 · Agentes de IA', description: 'Cuatro instituciones simuladas por IA deciden citando la ley. Cada cita está verificada y cada razonamiento es auditable.', side: 'bottom' } },
      { element: 'a.nav-link[data-value="vacios"]', popover: { title: '10 · Vacíos legales', description: 'Los 10 vacíos detectados, con evidencia, ejemplo internacional y propuesta. ¡Listo para explorar!', side: 'bottom' } },
    ];
  }
  window.iniciarTour = function () {
    if (!window.driver || !window.driver.js) { alert('No se pudo cargar la guía (sin conexión).'); return; }
    const d = window.driver.js.driver({
      showProgress: true, animate: true, allowClose: true, overlayOpacity: 0.65,
      nextBtnText: 'Siguiente →', prevBtnText: '← Anterior', doneBtnText: '¡Listo!', progressText: '{{current}} de {{total}}',
      steps: pasos()
    });
    d.drive();
  };
  // Primera visita: ofrecer la guía
  document.addEventListener('DOMContentLoaded', () => {
    let visto = null;
    try { visto = localStorage.getItem('sosnet_tour'); } catch (e) {}
    if (!visto) {
      setTimeout(() => {
        try { localStorage.setItem('sosnet_tour', '1'); } catch (e) {}
        const b = document.createElement('div');
        b.className = 'aviso-tour';
        b.innerHTML = '<b>¿Primera vez aquí?</b> Haz el recorrido guiado de 2 minutos. ' +
          '<button class="btn btn-sm btn-hero" id="aviso-si">Empezar</button> <button class="btn btn-sm btn-link" id="aviso-no">Ahora no</button>';
        document.body.appendChild(b);
        document.getElementById('aviso-si').onclick = () => { b.remove(); window.iniciarTour(); };
        document.getElementById('aviso-no').onclick = () => b.remove();
      }, 2500);
    }
  });
})();
