<script setup lang="ts">
/**
 * ImageMosaic — Magazine-style asymmetric photo grid.
 *
 * Three images in an asymmetric layout: one large spanning two rows
 * on the left, two stacked on the right. Optional caption overlay
 * on the large image.
 */

interface MosaicImage {
  src: string
  alt?: string
}

const props = defineProps<{
  images?: MosaicImage[]
  caption?: string
}>()

const imgs = computed(() => props.images ?? [])
</script>

<template>
  <div class="im-root">
    <div class="im-container">
      <div class="im-grid">
        <div class="im-cell im-a">
          <img v-if="imgs[0]" :src="imgs[0].src" :alt="imgs[0].alt ?? ''" class="im-img" />
          <div v-if="caption" class="im-caption-card">
            <p class="im-caption-text">{{ caption }}</p>
          </div>
        </div>
        <div class="im-cell im-b">
          <img v-if="imgs[1]" :src="imgs[1].src" :alt="imgs[1].alt ?? ''" class="im-img" />
        </div>
        <div class="im-cell im-c">
          <img v-if="imgs[2]" :src="imgs[2].src" :alt="imgs[2].alt ?? ''" class="im-img" />
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.im-root {
  width: 100vw;
  left: 50%;
  right: 50%;
  margin-left: -50vw;
  margin-right: -50vw;
  position: relative;
  padding: 0;
}

.im-container {
  max-width: 1400px;
  margin: 0 auto;
  padding: 0 2rem;
}

.im-grid {
  display: grid;
  grid-template-columns: 5fr 3fr;
  grid-template-rows: 1fr 1fr;
  gap: 0.5rem;
  height: clamp(360px, 45vw, 600px);
  border-radius: 1.25rem;
  overflow: hidden;
}

.im-a { grid-row: 1 / 3; grid-column: 1; }
.im-b { grid-row: 1; grid-column: 2; }
.im-c { grid-row: 2; grid-column: 2; }

.im-cell {
  position: relative;
  overflow: hidden;
}

.im-img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  object-position: center;
  display: block;
  transition: transform 0.5s ease;
}
.im-cell:hover .im-img {
  transform: scale(1.03);
}

.im-caption-card {
  position: absolute;
  bottom: 1.5rem;
  left: 1.5rem;
  right: 1.5rem;
  background: rgba(0, 0, 0, 0.45);
  backdrop-filter: blur(20px);
  -webkit-backdrop-filter: blur(20px);
  border-radius: 0.875rem;
  padding: 1rem 1.25rem;
}

.im-caption-text {
  margin: 0;
  font-size: 0.9rem;
  font-weight: 600;
  color: #fff;
  line-height: 1.45;
}

@media (max-width: 640px) {
  .im-container { padding: 0 1rem; }
  .im-grid {
    grid-template-columns: 1fr;
    grid-template-rows: 220px 140px 140px;
    height: auto;
  }
  .im-a { grid-row: 1; grid-column: 1; }
  .im-b { grid-row: 2; grid-column: 1; }
  .im-c { grid-row: 3; grid-column: 1; }
}
</style>
