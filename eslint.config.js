import eslint from '@eslint/js'
import prettierConfig from 'eslint-config-prettier'

export default [
    {
        ignores: ['node_modules/**', 'target/**', 'static/**', 'output/**', '.bsp/**', 'src/main/**'],
    },
    eslint.configs.recommended,
    {
        files: ['src/frontend/**/*.js'],
        languageOptions: {
            ecmaVersion: 'latest',
            sourceType: 'module',
            globals: {
                window: 'readonly',
                document: 'readonly',
                console: 'readonly',
                URLSearchParams: 'readonly',
                Chart: 'readonly',
            },
        },
        rules: {},
    },
    {
        files: ['vite.config.js', 'eslint.config.js', 'tailwind.config.js'],
        languageOptions: {
            ecmaVersion: 'latest',
            sourceType: 'module',
            globals: {
                __dirname: 'readonly',
            },
        },
        rules: {},
    },
    prettierConfig,
]
