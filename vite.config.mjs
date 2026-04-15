import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  plugins: [tailwindcss()],
  build: {
    emptyOutDir: true,
    outDir: ".tmp/vite",
    cssCodeSplit: false,
    rollupOptions: {
      input: "src/vite-entry.js",
      output: {
        assetFileNames: "tailwind.css",
        entryFileNames: "vite-entry.js"
      }
    }
  }
});
