create table public.models(
    id SERIAL PRIMARY KEY,
    title text not null,
    data JSON
);

insert into public.models (title,data) values ('Test 1','{ "some": "name", "value": 1}'), ('Test 2','{ "some": "thing", "value": 42}');




create table public.models( id SERIAL PRIMARY KEY, title text not null, data JSON );



insert into public.models (title,data) values ('Test 2','{ "some": "thing", "value": 42}');