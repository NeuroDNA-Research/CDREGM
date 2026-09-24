/* ══════════════════════════════════════════════════════════════
   CDREGM Web App — Shared header / sidebar / footer injector
   Each page defines `const SITE_ROOT = "";` (index.html) or `"../"`
   (pages/*.html) before loading this script, then calls
   initLayout("<activeId>") on DOMContentLoaded.
   ══════════════════════════════════════════════════════════════ */

const SECTIONS = [
  { id: "s1",  num: "01", slug: "s1-mystery-of-wealth.html",              title: "The Mystery of Wealth" },
  { id: "s2",  num: "02", slug: "s2-history-of-growth-theory.html",       title: "History of Growth Theory" },
  { id: "s3",  num: "03", slug: "s3-cdr-framework.html",                  title: "CDR Framework" },
  { id: "s4",  num: "04", slug: "s4-cdr-index-data.html",                 title: "CDR Index & Data" },
  { id: "s5",  num: "05", slug: "s5-graphical-evidence.html",             title: "Graphical Evidence" },
  { id: "s6",  num: "06", slug: "s6-statistical-model.html",              title: "Statistical Model" },
  { id: "s7",  num: "07", slug: "s7-entrepreneurship.html",               title: "Entrepreneurship" },
  { id: "s8",  num: "08", slug: "s8-global-time-invariance.html",         title: "Global Time Invariance" },
  { id: "s9",  num: "09", slug: "s9-growth-dynamics.html",                title: "Growth Dynamics" },
  { id: "s10", num: "10", slug: "s10-policy-case-studies.html",           title: "Policy & Case Studies" },
  { id: "s11", num: "11", slug: "s11-micro-macro-integration.html",       title: "Micro–Macro Integration" },
  { id: "s12", num: "12", slug: "s12-pedagogical-module.html",            title: "Pedagogical Module" },
  { id: "s13", num: "13", slug: "s13-human-collaboration-foundation.html",title: "Human Collaboration Foundation" },
  { id: "s14", num: "14", slug: "s14-ai-collaboration-catalyst.html",     title: "AI as Collaboration Catalyst" }
];

// Placeholder — the CDR Laboratory site URL was not available at build time.
const CDR_LAB_URL_PLACEHOLDER = "#cdr-lab-url-placeholder";

function pageHref(id, root) {
  if (id === "home") return root + "index.html";
  const s = SECTIONS.find(s => s.id === id);
  return root + "pages/" + s.slug;
}

function renderHeader(root, activeId) {
  return `
    <button class="nav-toggle" id="navToggle" aria-label="Toggle navigation">&#9776; Sections</button>
    <a href="${root}index.html" style="text-decoration:none; display:flex; align-items:center; gap:0.6rem;">
      <img class="logo-slot" src="${root}assets/img/fsu-logo-placeholder.svg" alt="Florida State University logo placeholder" title="FSU logo — placeholder">
      <img class="logo-slot" src="${root}assets/img/om-logo.png" alt="Universidad Dominicana O&amp;M logo" title="Universidad Dominicana O&amp;M">
      <span class="wordmark-group">
        <span class="wordmark"><span class="c">C</span><span class="d">D</span><span class="r">R</span> Growth Model</span>
        <span class="wordmark-sub">Capitalism &middot; Democracy &middot; Rule of Law</span>
      </span>
    </a>
    <span class="spacer"></span>
    <a class="header-link" href="${root}index.html">Home</a>
  `;
}

function renderSidebar(root, activeId) {
  const items = SECTIONS.map(s => {
    const active = s.id === activeId ? " active" : "";
    return `<a class="nav-item${active}" href="${root}pages/${s.slug}"><span class="num">&sect;${s.num}</span>${s.title}</a>`;
  }).join("\n");

  return `
    <div class="nav-group">
      <h3>Outline</h3>
      <a class="nav-item${activeId === "home" ? " active" : ""}" href="${root}index.html"><span class="num">&sect;0</span>App Overview</a>
      ${items}
    </div>
    <div class="sidebar-pdf">
      <h3>Source PDFs</h3>
      <span class="pdf-tag p1">P1 &middot; 4D CDR Growth Model<br>(Ridley &amp; Llaugel, 2022)</span>
      <span class="pdf-tag p2">P2 &middot; Advances in CDR Theory<br>(Ridley &amp; Llaugel, 2018)</span>
      <span class="pdf-tag p3">P3 &middot; A New CDR Index<br>(Ridley &amp; Khan)</span>
      <span class="pdf-tag p4">P4 &middot; Supply Side Unveiled<br>(Ridley / Llaugel)</span>
      <span class="pdf-tag p5">P5 &middot; AI in Human Collaboration<br>Skills (Ridley, Llaugel &amp; Garcia)</span>
    </div>
  `;
}

function renderFooter(root) {
  return `
    <h3>Source Documents</h3>
    <div class="pdf-ref p1"><strong>[P1] Ridley, A.D. &amp; Llaugel, F. (2022)</strong> &ldquo;The Four-Dimensional Scientific CDR Economic Growth Model: expected value, average, and limits to growth.&rdquo; 13 pp.</div>
    <div class="pdf-ref p2"><strong>[P2] Ridley, A.D. &amp; Llaugel, F. (2018)</strong> &ldquo;Advances in the CDR economic theory of entrepreneurship and GDP.&rdquo; 20 pp. + Appendices A&ndash;F.</div>
    <div class="pdf-ref p3"><strong>[P3] Ridley, A.D. &amp; Khan, A.</strong> &ldquo;A New CDR Index and its Implications for Entrepreneurship and Economic Growth.&rdquo; 20 pp. + Appendix A.</div>
    <div class="pdf-ref p4"><strong>[P4] Ridley, A.D.</strong> &ldquo;Advances in Capitalism/Democracy/Rule of law economic theory of Entrepreneurship: supply side unveiled.&rdquo; 20 pp. + Appendices A1&ndash;A3.</div>
    <div class="pdf-ref p5"><strong>[P5] Ridley, D., Llaugel, F., &amp; Garcia, C. A. (2026)</strong> &ldquo;Artificial Intelligence in Human Collaboration Skills Invocation, Recovery and Enhancement.&rdquo; <em>Theoretical Economics Letters</em>, 16(1), 249&ndash;268.</div>

    <div class="footer-credit-bar">
      <span>&copy; <span id="footerYear"></span> Dennis Ridley, Felipe Llaugel, Aryanne de Silva, Pierre Ngnepieba &amp; Abdullah Khan &middot; CDR theory and research. Web application design and educational content by NeuroDNA.</span>
      <a class="famu-link" href="${CDR_LAB_URL_PLACEHOLDER}" target="_blank" rel="noopener">
        FAMU Economics Department website &middot; visit the CDR Laboratory to run experiments
        <span class="arrow">&rarr;</span>
        <span class="placeholder-tag">link TBD</span>
      </a>
    </div>
  `;
}

function initLayout(activeId) {
  const root = typeof SITE_ROOT !== "undefined" ? SITE_ROOT : "";

  const headerMount = document.getElementById("site-header");
  if (headerMount) headerMount.innerHTML = renderHeader(root, activeId);

  const sidebarMount = document.getElementById("sidebar-mount");
  if (sidebarMount) sidebarMount.innerHTML = renderSidebar(root, activeId);

  const footerMount = document.getElementById("site-footer");
  if (footerMount) {
    footerMount.innerHTML = renderFooter(root);
    const yearEl = document.getElementById("footerYear");
    if (yearEl) yearEl.textContent = new Date().getFullYear();
  }

  // Prev/next pager (content pages only)
  const pagerMount = document.getElementById("pager-mount");
  if (pagerMount && activeId !== "home") {
    const idx = SECTIONS.findIndex(s => s.id === activeId);
    const prev = idx > 0 ? SECTIONS[idx - 1] : null;
    const next = idx < SECTIONS.length - 1 ? SECTIONS[idx + 1] : null;
    const prevHref = prev ? root + "pages/" + prev.slug : root + "index.html";
    const prevLabel = prev ? "&sect;" + prev.num + " " + prev.title : "App Overview";
    let html = `<a class="pager-link prev" href="${prevHref}"><div class="pager-dir">&larr; Previous</div><div class="pager-title">${prevLabel}</div></a>`;
    if (next) {
      html += `<a class="pager-link next" href="${root}pages/${next.slug}"><div class="pager-dir">Next &rarr;</div><div class="pager-title">&sect;${next.num} ${next.title}</div></a>`;
    } else {
      html += `<span></span>`;
    }
    pagerMount.innerHTML = html;
  }

  // Mobile nav toggle
  const toggleBtn = document.getElementById("navToggle");
  const sidebarEl = document.querySelector(".sidebar");
  if (toggleBtn && sidebarEl) {
    toggleBtn.addEventListener("click", () => sidebarEl.classList.toggle("open"));
    document.addEventListener("click", (e) => {
      if (window.innerWidth > 900) return;
      if (!sidebarEl.contains(e.target) && !toggleBtn.contains(e.target)) {
        sidebarEl.classList.remove("open");
      }
    });
  }
}
