#!/bin/sh
docker run --rm -p 8000:8000 -v `pwd`:/monolithic -w /monolithic -i -t erlang-flex bash
