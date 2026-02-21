<script setup lang="ts">
/**
 * AppleDocPage — Apple Developer Documentation-style page wrapper
 *
 * Renders a symbol/module documentation page inspired by
 * developer.apple.com/documentation/swiftui/navigationsplitview.
 * Lithos style — clean, no neo-noir grotesque effects.
 *
 * Usage in markdown:
 * ::apple-doc-page
 * ---
 * kind: Module          # "Module" | "File" | "Protocol" | "struct" | "class" | "enum"
 * framework: Lisp       # "Lisp" | "Swift"
 * availability: Hyalo 0.1+
 * abstract: One-sentence description shown under the title.
 * ---
 * Content (functions, tables, etc.)
 * ::
 */

interface Props {
  kind?: string
  framework?: string
  availability?: string
  abstract?: string
}

const props = withDefaults(defineProps<Props>(), {
  kind: 'Module',
  framework: '',
  availability: 'Hyalo 0.1+',
  abstract: ''
})

// Map kind to a color token for the badge
const kindColor = computed<string>(() => {
  const k = props.kind.toLowerCase()
  if (k === 'module' || k === 'file') return 'bg-blue-500/15 text-blue-600 dark:text-blue-400'
  if (k === 'struct' || k === 'class') return 'bg-orange-500/15 text-orange-600 dark:text-orange-400'
  if (k === 'protocol') return 'bg-purple-500/15 text-purple-600 dark:text-purple-400'
  if (k === 'enum') return 'bg-green-500/15 text-green-600 dark:text-green-400'
  return 'bg-gray-500/15 text-gray-600 dark:text-gray-400'
})
</script>

<template>
  <div class="apple-doc-page">
    <!-- Header badges -->
    <div class="doc-header">
      <div class="doc-meta">
        <span v-if="framework" class="doc-framework">{{ framework }}</span>
        <span class="doc-kind" :class="kindColor">{{ kind }}</span>
      </div>
      <p v-if="abstract" class="doc-abstract">{{ abstract }}</p>
      <div v-if="availability" class="doc-availability">
        <UIcon name="i-lucide-check-circle" class="availability-icon" />
        <span>{{ availability }}</span>
      </div>
    </div>

    <!-- Divider -->
    <div class="doc-divider" />

    <!-- Page content (slot) -->
    <div class="doc-body">
      <slot />
    </div>
  </div>
</template>

<style scoped>
.apple-doc-page {
  max-width: 860px;
}

.doc-header {
  padding-bottom: 1.5rem;
}

.doc-meta {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 0.75rem;
}

.doc-framework {
  font-size: 0.75rem;
  font-weight: 600;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  color: var(--ui-text-muted);
}

.doc-framework + .doc-kind::before {
  content: '·';
  margin-right: 0.5rem;
  color: var(--ui-text-muted);
}

.doc-kind {
  display: inline-block;
  font-size: 0.7rem;
  font-weight: 600;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  padding: 0.2em 0.6em;
  border-radius: 999px;
}

.doc-abstract {
  font-size: 1.05rem;
  color: var(--ui-text-muted);
  line-height: 1.5;
  margin: 0 0 0.75rem;
}

.doc-availability {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  font-size: 0.8rem;
  color: var(--ui-text-muted);
}

.availability-icon {
  width: 0.875rem;
  height: 0.875rem;
  color: var(--color-green-500);
  flex-shrink: 0;
}

.doc-divider {
  height: 1px;
  background: var(--ui-border);
  margin-bottom: 2rem;
}

.doc-body {
  /* Let Docus prose styles take over */
}

/* Override: tighten heading spacing inside doc body */
.doc-body :deep(h2) {
  font-size: 1.25rem;
  font-weight: 600;
  letter-spacing: -0.02em;
  margin-top: 2.5rem;
  margin-bottom: 0.75rem;
  padding-bottom: 0.4rem;
  border-bottom: 1px solid var(--ui-border);
}

.doc-body :deep(h3) {
  font-size: 1rem;
  font-weight: 600;
  margin-top: 1.5rem;
  margin-bottom: 0.4rem;
  font-family: var(--font-mono);
  color: var(--ui-text-highlighted);
}

.doc-body :deep(table) {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.875rem;
  margin: 1rem 0;
}

.doc-body :deep(th) {
  text-align: left;
  padding: 0.5rem 0.75rem;
  border-bottom: 2px solid var(--ui-border);
  font-weight: 600;
  font-size: 0.75rem;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  color: var(--ui-text-muted);
}

.doc-body :deep(td) {
  padding: 0.5rem 0.75rem;
  border-bottom: 1px solid var(--ui-border);
  vertical-align: top;
}

.doc-body :deep(td:first-child code) {
  font-size: 0.82rem;
  color: var(--ui-text-highlighted);
}
</style>
