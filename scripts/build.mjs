import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const tmpDir = path.join(root, ".tmp");
const distDir = path.join(root, "dist");
const elmOutput = path.join(tmpDir, "elm.js");
const cssOutput = path.join(tmpDir, "vite", "tailwind.css");
const templatePath = path.join(root, "src", "index.template.html");
mkdirSync(tmpDir, { recursive: true });
mkdirSync(distDir, { recursive: true });

runBinary("elm", ["make", "src/Main.elm", "--optimize", "--output", elmOutput]);
runBinary("vite", ["build", "--config", "vite.config.mjs"]);

const template = readFileSync(templatePath, "utf8");
const elmBundle = Buffer.from(readFileSync(elmOutput, "utf8"), "utf8").toString("base64");
const tailwindCss = escapeInlineStyle(readFileSync(cssOutput, "utf8"));

const html = template
  .replace("{{TAILWIND}}", tailwindCss)
  .replace("{{ELM}}", elmBundle);

writeFileSync(path.join(distDir, "index.html"), html);

function runBinary(binaryName, args) {
  const binName = process.platform === "win32" ? `${binaryName}.cmd` : binaryName;
  const binPath = path.join(root, "node_modules", ".bin", binName);

  if (!existsSync(binPath)) {
    throw new Error(`Missing ${binaryName}. Run "npm install" first so local devDependencies are available.`);
  }

  execFileSync(binPath, args, {
    cwd: root,
    stdio: "inherit"
  });
}

function escapeInlineStyle(value) {
  return value.replaceAll("</style>", "<\\/style>");
}
