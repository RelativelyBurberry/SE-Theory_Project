import { defineConfig } from 'vitest/config'
import react from '@vitejs/plugin-react'

// Vitest configuration (frontend). Test files live in src/__tests__/.
// jsdom provides a browser-like DOM for rendering React components.
export default defineConfig({
  plugins: [react()],
  test: {
    environment: 'jsdom',
    globals: true,
    setupFiles: ['./tests/setup.js'],
    css: false,
  },
})