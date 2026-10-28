import { useEffect, useRef, useState } from 'react'
import { searchTickers } from '../services/dataService'
import { normalizeTicker } from '../utils/ticker'

/**
 * Shared ticker autocomplete input (single implementation used by the ML
 * Portfolio Backtest page and the ML Model Comparison page).
 *
 * Behavior:
 *  - Debounced search (250 ms) after >= 2 characters, via searchTickers()
 *    (GET /market/tickers/search — real Yahoo Finance symbol lookup).
 *  - At most MAX_SUGGESTIONS (5) suggestions, from the backend response.
 *  - Keyboard: ArrowUp/ArrowDown move the highlight, Enter selects,
 *    Escape closes the dropdown; typing/arrow keys never block manual entry.
 *  - Clicking a suggestion fills the input (onSelect); clicking outside or
 *    Escape closes the dropdown.
 *  - No dropdown when there are no results; small inline loading indicator
 *    while a search is in flight.
 *  - Autocomplete is a convenience, never a hard dependency: if the search
 *    endpoint fails, the message is shown inline and manual typing/submission
 *    keeps working.
 *
 * Controlled component: `value`/`onChange` hold the raw text; `onSelect`
 * fires with the chosen { symbol, name } when the user picks a suggestion.
 *
 * Props:
 *  - value, onChange(value), onSelect({symbol, name})
 *  - id, placeholder, className (applied to the <input>), inputProps
 *  - onSelectBehavior: 'replace' (fill the field) | 'submit'
 *      'submit' still fills the field, then invokes onSubmit with the chosen
 *      symbol — used by the ML Portfolio page so selecting a suggestion adds
 *      it to the universe immediately.
 *  - onSubmit(symbol) — required when onSelectBehavior === 'submit'.
 *  - disabled
 */
export const MAX_SUGGESTIONS = 5
const DEBOUNCE_MS = 250
const MIN_QUERY_LENGTH = 2

export default function TickerAutocomplete({
  value,
  onChange,
  onSelect,
  onSelectBehavior = 'replace',
  onSubmit,
  id,
  placeholder,
  className,
  inputProps = {},
  disabled = false,
}) {
  const [suggestions, setSuggestions] = useState([])
  const [open, setOpen] = useState(false)
  const [loading, setLoading] = useState(false)
  const [searchError, setSearchError] = useState(null)
  const [highlight, setHighlight] = useState(-1)
  // The parent may pass its own onKeyDown (e.g. Enter adds the typed ticker
  // manually); it runs after the internal handling unless we already handled
  // the key (defaultPrevented).
  const { onKeyDown: parentOnKeyDown, ...restInputProps } = inputProps
  const rootRef = useRef(null)
  const requestSeq = useRef(0)
  const debounceRef = useRef(null)

  // Click outside closes the dropdown.
  useEffect(() => {
    if (!open) return undefined
    function handlePointerDown(e) {
      if (rootRef.current && !rootRef.current.contains(e.target)) {
        setOpen(false)
        setHighlight(-1)
      }
    }
    document.addEventListener('mousedown', handlePointerDown)
    document.addEventListener('touchstart', handlePointerDown)
    return () => {
      document.removeEventListener('mousedown', handlePointerDown)
      document.removeEventListener('touchstart', handlePointerDown)
    }
  }, [open])

  // Debounced lookup whenever the query changes.
  useEffect(() => {
    const query = normalizeTicker(value)
    if (debounceRef.current) clearTimeout(debounceRef.current)
    if (query.length < MIN_QUERY_LENGTH) {
      requestSeq.current += 1 // invalidate any in-flight request
      setSuggestions([])
      setOpen(false)
      setLoading(false)
      setSearchError(null)
      return undefined
    }
    setLoading(true)
    debounceRef.current = setTimeout(async () => {
      const seq = requestSeq.current + 1
      requestSeq.current = seq
      try {
        const data = await searchTickers(query, MAX_SUGGESTIONS)
        if (requestSeq.current !== seq) return // stale response
        const items = (data?.results ?? []).slice(0, MAX_SUGGESTIONS)
        setSuggestions(items)
        setSearchError(null)
        setOpen(items.length > 0)
        setHighlight(-1)
      } catch {
        if (requestSeq.current !== seq) return // stale response
        // Search unavailable: degrade gracefully — manual entry keeps working.
        setSuggestions([])
        setOpen(false)
        setSearchError('Ticker suggestions unavailable — you can still type a symbol manually.')
      } finally {
        if (requestSeq.current === seq) setLoading(false)
      }
    }, DEBOUNCE_MS)
    return () => clearTimeout(debounceRef.current)
  }, [value])

  useEffect(() => () => {
    if (debounceRef.current) clearTimeout(debounceRef.current)
  }, [])

  function choose(item) {
    const symbol = normalizeTicker(item.symbol)
    setSuggestions([])
    setOpen(false)
    setHighlight(-1)
    setSearchError(null)
    onChange(symbol)
    onSelect?.(item)
    if (onSelectBehavior === 'submit') {
      onSubmit?.(symbol)
    }
  }

  function handleKeyDown(e) {
    if (!open || suggestions.length === 0) {
      if (e.key === 'Escape') {
        setOpen(false)
      }
      return
    }
    if (e.key === 'ArrowDown') {
      e.preventDefault()
      setHighlight((h) => (h + 1) % suggestions.length)
    } else if (e.key === 'ArrowUp') {
      e.preventDefault()
      setHighlight((h) => (h <= 0 ? suggestions.length - 1 : h - 1))
    } else if (e.key === 'Enter') {
      // An exact typed symbol always wins (typing WIPRO.NS + Enter must add
      // WIPRO.NS even when the API lists WIPRO.BO first). Otherwise an
      // arrow-highlighted suggestion is chosen; with no highlight we defer to
      // the parent's manual-entry handling (never silently add a wrong symbol).
      const typed = normalizeTicker(value)
      const exact = typed && suggestions.find((s) => normalizeTicker(s.symbol) === typed)
      const item = exact ?? (highlight >= 0 ? suggestions[highlight] : null)
      if (item) {
        e.preventDefault()
        choose(item)
      }
    } else if (e.key === 'Escape') {
      e.preventDefault()
      setOpen(false)
      setHighlight(-1)
    }
  }

  return (
    <div className="relative" ref={rootRef}>
      <input
        id={id}
        type="text"
        value={value}
        disabled={disabled}
        onChange={(e) => onChange(e.target.value)}
        onKeyDown={(e) => {
          handleKeyDown(e)
          if (!e.defaultPrevented) parentOnKeyDown?.(e)
        }}
        onBlur={onSelectBehavior === 'submit' ? undefined : () => onChange(normalizeTicker(value))}
        placeholder={placeholder}
        autoComplete="off"
        role="combobox"
        aria-expanded={open}
        aria-controls={id ? `${id}-listbox` : undefined}
        aria-autocomplete="list"
        className={className}
        {...restInputProps}
      />
      {loading && (
        <span
          className="pointer-events-none absolute right-2 top-1/2 -translate-y-1/2 text-xs text-slate-400"
          data-testid="ticker-search-loading"
        >
          Searching…
        </span>
      )}
      {open && suggestions.length > 0 && (
        <ul
          id={id ? `${id}-listbox` : undefined}
          role="listbox"
          className="absolute z-20 mt-1 max-h-64 w-full overflow-auto rounded-md border border-slate-200 bg-white py-1 shadow-lg"
          data-testid="ticker-suggestions"
        >
          {suggestions.map((item, i) => (
            <li key={`${item.symbol}-${i}`} role="option" aria-selected={i === highlight}>
              <button
                type="button"
                // mousedown (not click) so selection wins over the input blur.
                onMouseDown={(e) => {
                  e.preventDefault()
                  choose(item)
                }}
                onMouseEnter={() => setHighlight(i)}
                className={`flex w-full items-baseline justify-between gap-3 px-3 py-2 text-left text-sm ${
                  i === highlight ? 'bg-emerald-50' : 'bg-white'
                }`}
                data-testid="ticker-suggestion"
              >
                <span className="shrink-0 font-medium text-slate-800">{item.symbol}</span>
                <span className="truncate text-xs text-slate-500">{item.name}</span>
              </button>
            </li>
          ))}
        </ul>
      )}
      {searchError && (
        <p className="mt-1 text-xs text-amber-700" data-testid="ticker-search-error">
          {searchError}
        </p>
      )}
    </div>
  )
}
