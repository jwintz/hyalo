<script setup lang="ts">
/**
 * MoreGrid — Apple-style "more features" grid.
 *
 * A responsive grid of compact feature items, each with an icon,
 * title, and short description. Inspired by the bottom section of
 * apple.com/fr/os/macos with small feature tiles.
 */

interface Item {
  icon?: string
  title: string
  description?: string
  to?: string
}

const props = defineProps<{
  heading?: string
  items?: Item[]
}>()
</script>

<template>
  <div class="mg-root">
    <h2 v-if="heading" class="mg-heading">{{ heading }}</h2>
    <div class="mg-grid">
      <component
        :is="item.to ? 'NuxtLink' : 'div'"
        v-for="item in items"
        :key="item.title"
        :to="item.to"
        class="mg-item"
        :class="{ 'mg-linked': item.to }"
      >
        <div v-if="item.icon" class="mg-icon-wrap">
          <UIcon :name="item.icon" class="mg-icon" />
        </div>
        <h3 class="mg-title">{{ item.title }}</h3>
        <p v-if="item.description" class="mg-desc">{{ item.description }}</p>
      </component>
    </div>
  </div>
</template>

<style scoped>
.mg-root {
  max-width: 1200px;
  margin: 0 auto;
  padding: 4rem 2rem 6rem;
}

.mg-heading {
  font-size: clamp(1.5rem, 3vw, 2.25rem);
  font-weight: 800;
  letter-spacing: -0.02em;
  color: var(--ui-text);
  text-align: center;
  margin: 0 0 3rem;
}

.mg-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
  gap: 2rem;
}

.mg-item {
  background: var(--ui-bg-elevated, var(--ui-bg));
  border: 1px solid var(--ui-border);
  border-radius: 1rem;
  padding: 1.75rem;
  transition: transform 0.2s ease, box-shadow 0.2s ease;
  text-decoration: none;
  color: inherit;
}
.mg-linked:hover {
  transform: translateY(-4px);
  box-shadow:
    0 8px 24px rgba(0, 0, 0, 0.06),
    0 2px 6px rgba(0, 0, 0, 0.03);
}

.mg-icon-wrap {
  width: 2.5rem;
  height: 2.5rem;
  display: flex;
  align-items: center;
  justify-content: center;
  margin-bottom: 1rem;
  border-radius: 0.625rem;
  background: color-mix(in srgb, var(--ui-primary) 12%, transparent);
}
.mg-icon {
  width: 1.25rem;
  height: 1.25rem;
  color: var(--ui-primary);
}

.mg-title {
  font-size: 1rem;
  font-weight: 700;
  color: var(--ui-text);
  margin: 0 0 0.5rem;
  line-height: 1.3;
}

.mg-desc {
  font-size: 0.875rem;
  line-height: 1.55;
  color: var(--ui-text-muted);
  margin: 0;
}

@media (max-width: 640px) {
  .mg-root { padding: 3rem 1.25rem 4rem; }
  .mg-grid { gap: 1.25rem; }
}
</style>
