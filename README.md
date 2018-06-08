# redis-hsmq

Why another (Redis based) Message Queue when there are so many out there? This project is part of the [2018 ZuriHac](https://2018.zurihac.info/) and originates from the need/desire to have a simple Message Queue with _some_ guarantees, but not as complicated to operate as RabbitMQ, ActiveMQ or others (for instance, [ActiveMQ](http://activemq.apache.org/masterslave.html) may require you to run Zookeeper, a shared file system such as a SAN or a shared Database if you want to run it in a Master/Slave mode). One of the goals of this project is to keep the required tech stack as simple as possible.

# Inspirations

This is heavily inspired by [RedisSMQ](https://github.com/weyoss/redis-smq) and tries to follow the [reliable queue pattern](https://redis.io/commands/rpoplpush).
