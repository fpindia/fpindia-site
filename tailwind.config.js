/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.hs", "./markdown/*.md"],
  theme: {
    extend: {},
  },
  plugins: [
    require('@tailwindcss/typography'),
    require('@tailwindcss/forms'),
    require('@tailwindcss/line-clamp'),
    require('@tailwindcss/aspect-ratio')
  ],
}

