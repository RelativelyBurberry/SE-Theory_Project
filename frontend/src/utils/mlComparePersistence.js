/**
 * ML Comparison frontend persistence (namespaced session storage).
 *
 * The completed POST /ml/compare response (comparison table, per-model
 * payloads, best_model, ROC/PR curve data) currently lives only in local
 * React state inside MLComparisonPage, so it is destroyed the moment the
 * route changes (e.g. /ml-compare -> /results) or the page refreshes.
 *
 * This module persists the RAW comparison response + the form it was run
 * with to sessionStorage under a single clearly-namespaced key. The page
 * restores from this snapshot on mount — a normal route transition remounts
 * the page and the table/charts/ROC curves come straight back without any
 * re-run, and a same-tab browser refresh behaves identically because
 * sessionStorage survives it.
 *
 * Storage is best-effort: if sessionStorage is unavailable (private mode,
 * quota), the comparison simply behaves as before (in-memory only) — the
 * page must never crash or silently drop the result because of this layer.
 *
 * Scope choice: sessionStorage (not localStorage) keeps the snapshot
 * bounded to the current tab/session. A completed comparison restored in a
 * future unrelated session would be misleading; the app's auth tokens are
 * the only thing that legitimately outlives the tab.
 */
export const ML_COMPARE_RESULT_KEY = 'alphaforge_ml_compare_result'

/** Persist the last completed comparison + the form that produced it. */
export function saveMlCompareSnapshot(snapshot) {
  try {
    sessionStorage.setItem(ML_COMPARE_RESULT_KEY, JSON.stringify(snapshot))
  } catch {
    // Best-effort: same-tab session storage unavailable — ignore.
  }
}

/**
 * Read the stored comparison snapshot, or null when absent/corrupt.
 * A snapshot is only usable when it carries a real result object and the
 * form it was run with.
 */
export function loadMlCompareSnapshot() {
  try {
    const raw = sessionStorage.getItem(ML_COMPARE_RESULT_KEY)
    if (!raw) return null
    const parsed = JSON.parse(raw)
    if (!parsed || !parsed.result || !parsed.form) return null
    return { result: parsed.result, form: parsed.form }
  } catch {
    return null
  }
}

/** Drop the stored snapshot (e.g. when a new comparison run starts). */
export function clearMlCompareSnapshot() {
  try {
    sessionStorage.removeItem(ML_COMPARE_RESULT_KEY)
  } catch {
    // Best-effort.
  }
}

export default { saveMlCompareSnapshot, loadMlCompareSnapshot, clearMlCompareSnapshot }