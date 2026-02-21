<script setup lang="ts">
/**
 * HighlightStrip — Sticky horizontal anchor tab navigation.
 *
 * A narrow bar that sticks to the top of the viewport. Each item
 * scrolls the page to the corresponding anchor. Modeled after
 * the Apple iPadOS highlights pill strip.
 */

interface Item {
  label: string
  anchor: string
}

const props = defineProps<{
  items?: Item[]
}>()

function scrollTo(anchor: string) {
  const el = document.getElementById(anchor)
  if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' })
}
</script>

<template>
  <div class="hs-root">
    <ul class="hs-list">
      <li
        v-for="item in items"
        :key="item.anchor"
        class="hs-item"
        @click="scrollTo(item.anchor)"
      >
        {{ item.label }}
      </li>
    </ul>
  </div>
</template>

<style scoped>
.hs-root {
  position: sticky;
  top: 0;
  z-index: 40;
  width: 100vw;
  left: 50%;
  right: 50%;
  margin-left: -50vw;
  margin-right: -50vw;
  background: color-mix(in srgb, var(--ui-bg) 85%, transparent);
  backdrop-filter: blur(16px);
  -webkit-backdrop-filter: blur(16px);
  border-bottom: 1px solid var(--ui-border);
}

.hs-list {
  display: flex;
  flex-wrap: nowrap;
  overflow-x: auto;
  gap: 0;
  list-style: none;
  margin: 0;
  padding: 0 1rem;
  justify-content: center;
  scrollbar-width: none;
}
.hs-list::-webkit-scrollbar { display: none; }

.hs-item {
  flex-shrink: 0;
  padding: 0.875rem 1.25rem;
  font-size: 0.8rem;
  font-weight: 500;
  color: var(--ui-text-muted);
  cursor: pointer;
  transition: color 0.15s ease;
  white-space: nowrap;
  border-bottom: 2px solid transparent;
}
.hs-item:hover {
  color: var(--ui-primary);
  border-bottom-color: var(--ui-primary);
}
</style>
