package com.proyect.masterdata.repository;


import com.proyect.masterdata.domain.User;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserRepository extends JpaRepository<User, String> {
}
