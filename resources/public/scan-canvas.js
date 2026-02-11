class ScanCanvas extends HTMLElement {
  static get observedAttributes() {
    return ["width", "height", "pixel-size", "base-color", "pixels"];
  }

  constructor() {
    super();
    this.attachShadow({ mode: "open" });

    this._canvas = document.createElement("canvas");
    this._ctx = this._canvas.getContext("2d", { alpha: true });

    const style = document.createElement("style");
    style.textContent = `
      :host { display: inline-block; }
      canvas { display: block; }
    `;

    this.shadowRoot.append(style, this._canvas);
  }

  connectedCallback() {
    this.render();
  }

  attributeChangedCallback() {
    this.render();
  }

  // --- Public properties (optional, but convenient) ---
  get widthPx() { return this._numAttr("width", 0); }
  set widthPx(v) { this.setAttribute("width", String(v)); }

  get heightPx() { return this._numAttr("height", 0); }
  set heightPx(v) { this.setAttribute("height", String(v)); }

  get pixelSize() { return this._numAttr("pixel-size", 8); }
  set pixelSize(v) { this.setAttribute("pixel-size", String(v)); }

  get baseColor() { return this.getAttribute("base-color") ?? "#00ff88"; }
  set baseColor(v) { this.setAttribute("base-color", v); }

  get pixels() { return this._parsePixels(this.getAttribute("pixels")); }
  set pixels(arr) { this.setAttribute("pixels", JSON.stringify(arr)); }

  // --- Helpers ---
  _numAttr(name, fallback) {
    const raw = this.getAttribute(name);
    const n = raw == null ? NaN : Number(raw);
    return Number.isFinite(n) ? n : fallback;
  }

  _parsePixels(raw) {
    if (!raw) return [];
    // Accept JSON array string OR a comma/space-separated list.
    try {
      const parsed = JSON.parse(raw);
      return Array.isArray(parsed) ? parsed.map(Number) : [];
    } catch {
      return raw
        .split(/[,\s]+/)
        .filter(Boolean)
        .map(Number);
    }
  }

  _cssColorWithAlpha(color, alpha) {
    // Use the modern "color-mix" if available? Not reliable everywhere.
    // Safer: set fillStyle to color, then set globalAlpha.
    // We'll do globalAlpha externally in draw loop.
    return color;
  }

  render() {
    const widthPx = this.widthPx;
    const heightPx = this.heightPx;
    const pixelSize = this.pixelSize || 8;
    const baseColor = this.baseColor;
    const pixels = this.pixels;

    if (widthPx <= 0 || heightPx <= 0 || pixelSize <= 0) {
      // Still ensure canvas exists but do nothing.
      this._canvas.width = 0;
      this._canvas.height = 0;
      return;
    }

    // Physical canvas size (in CSS pixels). If you want retina scaling, can add dpr scaling.
    this._canvas.width = widthPx;
    this._canvas.height = heightPx;
    this._canvas.style.width = `${widthPx}px`;
    this._canvas.style.height = `${heightPx}px`;

    const cols = Math.floor(widthPx / pixelSize);
    const rows = Math.floor(heightPx / pixelSize);
    const cellCount = cols * rows;

    const ctx = this._ctx;
    ctx.clearRect(0, 0, widthPx, heightPx);

    if (cols <= 0 || rows <= 0 || cellCount <= 0) return;

    // Normalize: max non-zero => alpha 1.0
    let maxVal = 0;
    const len = Math.min(pixels.length, cellCount);
    for (let i = 0; i < len; i++) {
      const v = pixels[i] || 0;
      if (v > maxVal) maxVal = v;
    }

    if (maxVal <= 0) return;

    ctx.fillStyle = this._cssColorWithAlpha(baseColor, 1);

    // Draw
    for (let i = 0; i < len; i++) {
      const v = pixels[i] || 0;
      if (v <= 0) continue;

      // Linear normalization: v/maxVal => [0..1]
      const alpha = Math.max(0, Math.min(1, v / maxVal));
      if (alpha <= 0) continue;

      const x = (i % cols) * pixelSize;
      const y = Math.floor(i / cols) * pixelSize;

      ctx.globalAlpha = alpha;
      ctx.fillRect(x, y, pixelSize, pixelSize);
    }

    ctx.globalAlpha = 1;
  }
}

customElements.define("scan-canvas", ScanCanvas);
