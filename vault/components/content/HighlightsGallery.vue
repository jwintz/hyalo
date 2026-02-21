<script setup lang="ts">
/**
 * HighlightsGallery — Apple-style horizontal scrollable pill nav.
 *
 * A single row of clickable pills that scroll to anchor sections.
 * Inspired by the macOS/iPadOS highlights strip at the top of apple.com.
 * Each item is a compact card with title + description + thumbnail.
 */

interface Item {
  title: string
  description?: string
  image?: string
  anchor?: string
}

const props = defineProps<{
  items?: Item[]
}>()

function scrollTo(anchor?: string) {
  if (!anchor) return
  const el = document.getElementById(anchor)
  if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' })
}
</script>

<template>
  <div class="hg-root">
    <div class="hg-track">
      <article
        v-for="item in items"
        :key="item.title"
        class="hg-card"
        :class="{ 'hg-clickable': item.anchor }"
        @click="scrollTo(item.anchor)"
      >
        <div class="hg-thumb">
          <img v-if="item.image" :src="item.image" :alt="item.title" class="hg-img" />
        </div>
        <div class="hg-text">
          <h3 class="hg-title">{{ item.title }}</h3>
          <p v-if="item.description" class="hg-desc">{{ item.description }}</p>
        </div>
      </article>
    </div>
  </div>
</template>

<style scoped>
.hg-root {
  width: 100vw;
  left: 50%;
  right: 50%;
  margin-left: -50vw;
  margin-right: -50vw;
  position: relative;
  padding: 3rem 0 4rem;
  background: var(--ui-bg);
}

.hg-track {
  display: flex;
  gap: 1.25rem;
  overflow-x: auto;
  scroll-snap-type: x mandatory;
  padding: 0 max(2rem, calc((100vw - 1200px) / 2));
  scrollbar-width: none;
}
.hg-track::-webkit-scrollbar { display: none; }

.hg-card {
  flex-shrink: 0;
  width: 300px;
  scroll-snap-align: start;
  background: var(--ui-bg-elevated, var(--ui-bg));
  border-radius: 1.25rem;
  overflow: hidden;
  border: 1px solid var(--ui-border);
  transition: transform 0.25s ease, box-shadow 0.25s ease;
}
.hg-clickable { cursor: pointer; }
.hg-card:hover {
  transform: translateY(-6px);
  box-shadow:
    0 8px 30px rgba(0, 0, 0, 0.08),
    0 2px 8px rgba(0, 0, 0, 0.04);
}

.hg-thumb {
  width: 100%;
  height: 180px;
  overflow: hidden;
}
.hg-img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  display: block;
  transition: transform 0.4s ease;
}
.hg-card:hover .hg-img {
  transform: scale(1.04);
}

.hg-text {
  padding: 1.25rem 1.25rem 1.5rem;
}

.hg-title {
  font-size: 1rem;
  font-weight: 700;
  color: var(--ui-text);
  margin: 0;
  line-height: 1.3;
}

.hg-desc {
  font-size: 0.85rem;
  line-height: 1.5;
  color: var(--ui-text-muted);
  margin: 0.5rem 0 0;
}

@media (max-width: 640px) {
  .hg-card { width: 260px; }
  .hg-thumb { height: 150px; }
}
</style>
