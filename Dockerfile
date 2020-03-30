FROM python:3.7-alpine
WORKDIR /code
COPY requirements.txt requirements.txt
RUN pip install -r requirements.txt
COPY monocle monocle
COPY setup.py setup.py
RUN python setup.py install
