// esbuild.config.mjs
import { build } from 'esbuild';
import { copyFile, mkdir, rm } from 'node:fs/promises';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const distDir = join(__dirname, 'dist');

await rm(distDir, { recursive: true, force: true });
await mkdir(distDir, { recursive: true });

// Keep runtime deps external so they resolve from the installed node_modules/
// at launch time. Bundling these triggers dynamic-require issues.
const SERVER_EXTERNALS = [
  '@anthropic-ai/claude-agent-sdk',
  'marked',
  'highlight.js',
  'isomorphic-dompurify',
  'jsdom',
];

// Backend bundle
await build({
  entryPoints: [join(__dirname, 'src/server.ts')],
  outfile: join(distDir, 'server.js'),
  bundle: true,
  platform: 'node',
  target: 'node20',
  format: 'esm',
  banner: { js: "import { createRequire } from 'module';const require = createRequire(import.meta.url);" },
  external: SERVER_EXTERNALS,
});

// Frontend bundle — dompurify (not isomorphic-dompurify) bundled into client
await build({
  entryPoints: [join(__dirname, 'src/web/app.ts')],
  outfile: join(distDir, 'app.js'),
  bundle: true,
  platform: 'browser',
  target: 'es2022',
  format: 'esm',
});

// Static files
await copyFile(join(__dirname, 'src/web/index.html'), join(distDir, 'index.html'));
await copyFile(join(__dirname, 'src/web/app.css'), join(distDir, 'app.css'));

console.log('dist/ rebuilt');
