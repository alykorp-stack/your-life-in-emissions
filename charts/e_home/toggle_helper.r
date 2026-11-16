# Two-way (or more) switch for an echarts4r widget, used by the home page charts 01 and 02.
#
#   add_toggle(widget, choices, default, update_js, data)
#     choices    named character vector, key = mode id, value = button label, e.g. c(weight = "By weight")
#     default    mode shown when the page opens
#     update_js  JavaScript function(chart, mode, d) that applies a mode, e.g. with chart.setOption()
#     data       list passed to update_js as d
# The current mode is also kept in window.chartMode, so tooltip formatters can read it.
# The switch sits top left, under the title (top: 66px); leave grid top room for it.

library(htmlwidgets)

add_toggle <- function(widget, choices, default, update_js, data = list()) {
  data$choices <- unname(map2(names(choices), unname(choices), \(k, l) list(k, l)))
  data$default <- default
  onRender(widget, paste0("
    function(el, x, d) {
      var chart = echarts.getInstanceByDom(el);
      var update = ", update_js, ";
      var css = document.createElement('style');
      css.textContent = `
        .chart-toggle { position: absolute; top: 66px; left: 0; z-index: 5; display: inline-flex;
                        border: 2px solid #2b6cb0; border-radius: 12px; overflow: hidden;
                        font-family: Inter, system-ui, sans-serif; }
        .chart-toggle button { font: 700 15px Inter, system-ui, sans-serif; height: 40px; padding: 0 18px; border: 0;
                               background: #f2f7fd; color: #1e3a5f; cursor: pointer; transition: background .15s; }
        .chart-toggle button + button { border-left: 2px solid #2b6cb0; }
        .chart-toggle button:hover { background: #e6f0fb; }
        .chart-toggle button[aria-pressed=true] { background: #2b6cb0; color: #fff; }
        .chart-toggle button:focus-visible { outline: 3px solid #93c5fd; outline-offset: -3px; }
      `;
      document.head.appendChild(css);
      el.style.position = 'relative';

      var box = document.createElement('div');
      box.className = 'chart-toggle';
      box.setAttribute('role', 'group');
      var buttons = {};
      d.choices.forEach(function(c) {
        var b = document.createElement('button');
        b.textContent = c[1];
        b.addEventListener('click', function() { set(c[0]); });
        buttons[c[0]] = b;
        box.appendChild(b);
      });
      el.appendChild(box);

      function set(mode) {
        window.chartMode = mode;
        Object.keys(buttons).forEach(function(k) { buttons[k].setAttribute('aria-pressed', k === mode); });
        update(chart, mode, d);
      }
      set(d.default);
    }
  "), data = data)
}
