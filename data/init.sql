create table public.models(
    id SERIAL PRIMARY KEY,
    title text not null,
    data JSON
);
