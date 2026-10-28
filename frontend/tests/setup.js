import '@testing-library/jest-dom/vitest'

// jsdom does not implement localStorage by default in some configurations;
// provide a stable in-memory implementation for the tests.
class MemoryStorage {
  constructor() {
    this.store = new Map()
  }
  getItem(key) {
    return this.store.has(key) ? this.store.get(key) : null
  }
  setItem(key, value) {
    this.store.set(key, String(value))
  }
  removeItem(key) {
    this.store.delete(key)
  }
  clear() {
    this.store.clear()
  }
}

if (!window.localStorage) {
  Object.defineProperty(window, 'localStorage', { value: new MemoryStorage() })
}