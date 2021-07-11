# Haskell + GraphQL + Elm example

## Setup

### Start Postgres in a docker container
```shell-script
docker run -p 5432:5432 -e POSTGRES_PASSWORD=secret -d postgres
```

### Populate test data
```sql
create database experiments;

create table projection_patients (id text not null primary key, first_name text not null, last_name text not null, data jsonb not null);

insert into projection_patients (id, first_name, last_name, "data")
values ('Z0GK9wJ1cleInzbPfWwKDQ',' John','Doe', '{"id":"Z0GK9wJ1cleInzbPfWwKDQ","insurances":[{"code":"CL-12345-67890","isExpired":false,"plan":"Primary"}],"person":{"firstName":"John","emailAddress":"john@gmail.com","phoneNumber":null,"lastName":"Bergsma"}}');

insert into projection_patients (id, first_name, last_name, "data")
values ('DLzG0edIlgJpl2LpPwzqAQ',' Jane','Doe', '{"id":"DLzG0edIlgJpl2LpPwzqAQ","insurances":[{"code":"SL-12345-67890","isExpired":false,"plan":"Primary"}],"person":{"firstName":"Jane","emailAddress":"jane@msn.com","phoneNumber":null,"lastName":"Doe"}}');

```
_(use a tool like DataGrip to connect to your Postgres instance)_

### Build Elm
```shell-script
cd public
./build.sh
```

### Build Haskell
```shell-script
cabal install
cabal build
cabal run
```