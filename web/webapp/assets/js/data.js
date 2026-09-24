/* ══════════════════════════════════════════════════════════════
   CDREGM Web App — Shared model data & formulas
   All coefficients and the 8-country table are taken verbatim from
   web/webapp_outline.html (§4, §6, §9), sourced from P1–P4.
   ══════════════════════════════════════════════════════════════ */

/* 79-country study; the 8 rows below are the published example rows
   reproduced in the outline (§4 Table). Full panel: P1 Tables 1–2 / P3 Table 1. */
const CDR_COUNTRIES = [
  { name: "Singapore",     flag: "🇸🇬", gdp: 83066, C: 0.576, D: 0.517, R: 0.960, cdr: 0.829, N: 0.000 },
  { name: "Hong Kong",     flag: "🇭🇰", gdp: 55097, C: 0.866, D: 0.919, R: 0.927, cdr: 0.774, N: 0.000 },
  { name: "United States", flag: "🇺🇸", gdp: 51281, C: 0.445, D: 0.913, R: 0.907, cdr: 0.572, N: 0.017 },
  { name: "Norway",        flag: "🇳🇴", gdp: 67166, C: 0.373, D: 0.980, R: 0.973, cdr: 0.501, N: 0.172 },
  { name: "Japan",         flag: "🇯🇵", gdp: 37519, C: 0.223, D: 0.899, R: 0.920, cdr: 0.456, N: 0.000 },
  { name: "China",         flag: "🇨🇳", gdp: 13224, C: 0.021, D: 0.195, R: 0.460, cdr: 0.162, N: 0.018 },
  { name: "Nigeria",       flag: "🇳🇬", gdp: 6054,  C: 0.000, D: 0.396, R: 0.247, cdr: 0.112, N: 0.023 },
  { name: "Russia",        flag: "🇷🇺", gdp: 24449, C: 0.046, D: 0.128, R: 0.253, cdr: 0.145, N: 0.110 }
];

// Standardization bounds used in Applied Exercise 2 (§12)
const G_MAX = 83066; // Singapore
const G_MIN = 1112;

// OLS Stage-1 model (§6): ĝ = C·C + D·D + R·R + CDR·(C·D·R) + N·N ,  R²adj = 83%, n = 79
const OLS = { C: 1.53, D: 0.14, R: 0.23, CDR: -1.21, N: 0.38, R2: 0.83 };

// First-stage 2SLS (§6): ê = const + L·L + D·D + R·R + CDR·(C·D·R) − N·N ,  R²adj = 95%
const STAGE1 = { const: 0.04, L: -0.07, D: -0.16, R: 0.22, CDR: 1.11, N: -0.02, R2: 0.95 };

// Second-stage 2SLS (§6): ĝ = ê·ehat + D·D + R·R + CDR·(ê·D·R) + N·N ,  R²adj = 74%
const STAGE2 = { ehat: 1.30, D: 0.12, R: 0.28, CDR: -0.98, N: 0.39, R2: 0.74 };

// Parametric-derivation coefficients (§9), P1 §4.1–4.3
const GROWTH_PARAMS = {
  beta0: -0.00051,
  betaC: 1.534346,
  betaEhat: 1.295617,
  betaD: 0.116963,
  betaR: 0.275395354,
  betaCDR: -0.98133,
  betaN: 0.388146,
  ehatRef: 0.85
};

function clamp01(x) { return Math.max(0, Math.min(1, x)); }

function olsGHat({ C, D, R, N }) {
  return OLS.C * C + OLS.D * D + OLS.R * R + OLS.CDR * (C * D * R) + OLS.N * N;
}

function stage2GHat({ ehat, D, R, N }) {
  return STAGE2.ehat * ehat + STAGE2.D * D + STAGE2.R * R + STAGE2.CDR * (ehat * D * R) + STAGE2.N * N;
}

// Inverse of g = (G - Gmin)/(Gmax - Gmin)  ->  G = g*(Gmax-Gmin) + Gmin
function inverseGDP(g, gmax = G_MAX, gmin = G_MIN) {
  return g * (gmax - gmin) + gmin;
}

// Expected endogenous growth (§9, P1 §4.1) — constant, does not vary with ê
function expectedGrowth(p = GROWTH_PARAMS) {
  return 0.5 * (p.beta0 + (p.betaC - p.betaEhat) + p.betaD + p.betaR + p.betaCDR + p.betaN);
}

// Theoretical maximum endogenous growth as a function of ê (§9, P1 §4.3)
function maxGrowth(ehat, p = GROWTH_PARAMS) {
  return p.beta0
    + (p.betaC - p.betaEhat) / 2
    + p.betaD / 2
    + p.betaR / 2
    + (p.betaCDR * ehat) / 4
    + p.betaN / 2;
}

function fmtPct(x, digits = 1) { return (x * 100).toFixed(digits) + "%"; }
function fmtUSD(x) { return "$" + Math.round(x).toLocaleString("en-US"); }
