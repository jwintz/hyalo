<script setup lang="ts">
/**
 * SectionIntro — Centered section heading with label, title, and body.
 *
 * Apple-style section opener: a small label, large heading, and optional
 * body paragraph — all centered. Used to introduce each major section
 * on the landing page.
 */

const props = defineProps<{
  label?: string
  title?: string
  accent?: string
  body?: string
}>()
</script>

<template>
  <div class="si-root">
    <span v-if="label" class="si-label">{{ label }}</span>
    <h2 class="si-title">
      <span class="si-title-main">{{ title }}</span>
      <br v-if="accent" />
      <span v-if="accent" class="si-title-accent">{{ accent }}</span>
    </h2>
    <p v-if="body" class="si-body">{{ body }}</p>
    <slot />
  </div>
</template>

<style scoped>
.si-root {
  max-width: 740px;
  margin: 0 auto;
  padding: 5rem 2rem 3rem;
  text-align: center;
}

.si-label {
  display: inline-block;
  font-size: 0.75rem;
  font-weight: 600;
  letter-spacing: 0.14em;
  text-transform: uppercase;
  color: var(--ui-primary);
  margin-bottom: 1rem;
}

.si-title {
  font-size: clamp(2rem, 5vw, 3.5rem);
  font-weight: 800;
  letter-spacing: -0.03em;
  line-height: 1.1;
  margin: 0;
}
.si-title-main { color: var(--ui-text); }
.si-title-accent { color: var(--ui-primary); }

.si-body {
  font-size: clamp(1rem, 1.8vw, 1.15rem);
  line-height: 1.7;
  color: var(--ui-text-muted);
  margin: 1.5rem auto 0;
  max-width: 56ch;
}

@media (max-width: 640px) {
  .si-root { padding: 3rem 1.5rem 2rem; }
}
</style>
