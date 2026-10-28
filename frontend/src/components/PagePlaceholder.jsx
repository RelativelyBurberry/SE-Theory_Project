/**
 * Generic placeholder page used by scaffolded screens that have not been
 * implemented yet. Replaced by real content in later phases.
 */
export default function PagePlaceholder({ title, description }) {
  return (
    <section className="rounded-lg border border-dashed border-slate-300 bg-white p-8 text-center">
      <h2 className="text-xl font-semibold text-slate-800">{title}</h2>
      <p className="mx-auto mt-2 max-w-xl text-sm text-slate-500">
        {description}
      </p>
    </section>
  )
}