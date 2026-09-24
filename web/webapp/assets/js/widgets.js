/* ══════════════════════════════════════════════════════════════
   CDREGM Web App — Reusable interactive widgets
   Quiz, Poll (localStorage-backed demo poll), sortable table,
   and case-study filter buttons. No backend — everything runs
   client-side so the site works from a plain file:// open too.
   ══════════════════════════════════════════════════════════════ */

/* ---------- Quiz ---------- */
// questions: [{ q, options: [...], correct: index, explain }]
function mountQuiz(container, questions) {
  container.innerHTML = "";
  questions.forEach((item, qi) => {
    const box = document.createElement("div");
    box.className = "quiz-item";
    const qText = document.createElement("div");
    qText.className = "q-text";
    qText.innerHTML = `<strong>Q${qi + 1}.</strong> ${item.q}`;
    box.appendChild(qText);

    const opts = document.createElement("div");
    opts.className = "quiz-options";
    const feedback = document.createElement("div");
    feedback.className = "quiz-feedback";

    let answered = false;
    item.options.forEach((optText, oi) => {
      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = "quiz-option";
      btn.textContent = optText;
      btn.addEventListener("click", () => {
        if (answered) return;
        answered = true;
        const buttons = opts.querySelectorAll(".quiz-option");
        buttons.forEach((b, bi) => {
          b.disabled = true;
          if (bi === item.correct) b.classList.add("correct");
          else if (bi === oi) b.classList.add("incorrect");
        });
        feedback.textContent = (oi === item.correct ? "Correct. " : "Not quite. ") + (item.explain || "");
      });
      opts.appendChild(btn);
    });

    box.appendChild(opts);
    box.appendChild(feedback);
    container.appendChild(box);
  });
}

/* ---------- Poll (per-browser demo, localStorage) ---------- */
function mountPoll(container, pollId, options) {
  const countsKey = "cdregm_poll_" + pollId + "_counts";
  const votedKey = "cdregm_poll_" + pollId + "_voted";

  function readCounts() {
    try {
      const raw = localStorage.getItem(countsKey);
      if (raw) return JSON.parse(raw);
    } catch (e) {}
    return options.map(() => 0);
  }
  function writeCounts(counts) {
    try { localStorage.setItem(countsKey, JSON.stringify(counts)); } catch (e) {}
  }
  function hasVoted() {
    try { return localStorage.getItem(votedKey) !== null; } catch (e) { return false; }
  }
  function votedIndex() {
    try { return parseInt(localStorage.getItem(votedKey), 10); } catch (e) { return -1; }
  }

  function render() {
    const counts = readCounts();
    const total = counts.reduce((a, b) => a + b, 0);
    const voted = hasVoted();
    const myVote = votedIndex();

    const wrap = document.createElement("div");
    wrap.className = "poll-options";

    options.forEach((label, i) => {
      const pct = total > 0 ? Math.round((counts[i] / total) * 100) : 0;
      const row = document.createElement("div");
      row.className = "poll-option";
      row.innerHTML = `<div class="poll-fill" style="width:${voted ? pct : 0}%"></div>
        <div class="poll-label"><span>${label}${i === myVote ? " &middot; your vote" : ""}</span>${voted ? `<span class="poll-pct">${pct}%</span>` : ""}</div>`;
      if (!voted) {
        row.style.cursor = "pointer";
        row.addEventListener("click", () => {
          const c = readCounts();
          c[i] += 1;
          writeCounts(c);
          try { localStorage.setItem(votedKey, String(i)); } catch (e) {}
          render();
        });
      }
      wrap.appendChild(row);
    });

    const total2 = readCounts().reduce((a, b) => a + b, 0);
    const totalEl = document.createElement("div");
    totalEl.className = "poll-total";
    totalEl.textContent = voted
      ? `${total2} response${total2 === 1 ? "" : "s"} recorded in this browser (demo poll — not shared across visitors).`
      : "Click an option to cast your vote (demo poll — stored only in this browser).";

    container.innerHTML = "";
    container.appendChild(wrap);
    container.appendChild(totalEl);
  }

  render();
}

/* ---------- Sortable table ---------- */
function makeSortable(table) {
  const headers = table.querySelectorAll("thead th");
  const tbody = table.querySelector("tbody");
  headers.forEach((th, colIndex) => {
    let asc = true;
    const arrow = document.createElement("span");
    arrow.className = "sort-arrow";
    arrow.textContent = "⇅";
    th.appendChild(arrow);
    th.addEventListener("click", () => {
      const rows = Array.from(tbody.querySelectorAll("tr"));
      rows.sort((r1, r2) => {
        const v1 = r1.children[colIndex].dataset.sort ?? r1.children[colIndex].textContent.trim();
        const v2 = r2.children[colIndex].dataset.sort ?? r2.children[colIndex].textContent.trim();
        const n1 = parseFloat(v1), n2 = parseFloat(v2);
        let cmp;
        if (!isNaN(n1) && !isNaN(n2)) cmp = n1 - n2;
        else cmp = String(v1).localeCompare(String(v2));
        return asc ? cmp : -cmp;
      });
      rows.forEach(r => tbody.appendChild(r));
      headers.forEach(h => h.querySelector(".sort-arrow") && (h.querySelector(".sort-arrow").textContent = "⇅"));
      arrow.textContent = asc ? "↑" : "↓";
      asc = !asc;
    });
  });
}

/* ---------- Case-study filter buttons ---------- */
function mountFilter(filterBar, cards, verdictAttr = "data-verdict") {
  const buttons = filterBar.querySelectorAll(".filter-btn");
  buttons.forEach(btn => {
    btn.addEventListener("click", () => {
      buttons.forEach(b => b.classList.remove("active"));
      btn.classList.add("active");
      const val = btn.dataset.filter;
      cards.forEach(card => {
        const show = val === "all" || card.getAttribute(verdictAttr) === val;
        card.classList.toggle("hidden-by-filter", !show);
      });
    });
  });
}
