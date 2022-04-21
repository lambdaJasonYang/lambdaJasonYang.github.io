---
title: Quick Springboot 2
tags: prog, bean, java, QuickCode
---

# Repository pattern

Repository pattern allows us to focus on the model instead of the DB. We can easily swap DB by modifying configs and everything should still work.

```{.java filename=room.java}
@Entity
@Table(name="ROOM")
public class Room {
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	@Column(name="ROOM_ID")
	private long id;
	
	@Column(name="NAME")
	private String name;
	
	@Column(name="ROOM_NUMBER")
	private String roomNumber;
	
	@Column(name="BED_INFO")
	private String bedInfo;

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getRoomNumber() {
		return roomNumber;
	}

	public void setRoomNumber(String roomNumber) {
		this.roomNumber = roomNumber;
	}

	public String getBedInfo() {
		return bedInfo;
	}

	public void setBedInfo(String bedInfo) {
		this.bedInfo = bedInfo;
	}

	@Override
	public String toString() {
		return "Room [id=" + id + ", name=" + name + ", roomNumber=" + roomNumber + ", bedInfo=" + bedInfo + "]";
	}

}
```

```{.java filename=roomrepo.java}
package com.example.demoSpring.data;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface RoomRepository extends CrudRepository<Room, Long> {

}

```

Using the room repo

```java
@Component
public class AppStartupEvent implements ApplicationListener<ApplicationReadyEvent> {
	private final RoomRepository roomRepository;
	public AppStartupEvent(RoomRepository roomRepository) {
		super();
		this.roomRepository = roomRepository;
	}
	
	@Override
	public void onApplicationEvent(ApplicationReadyEvent event) {
		// TODO Auto-generated method stub
		Iterable<Room> rooms = this.roomRepository.findAll();
		rooms.forEach(System.out::println);
	}

}

```