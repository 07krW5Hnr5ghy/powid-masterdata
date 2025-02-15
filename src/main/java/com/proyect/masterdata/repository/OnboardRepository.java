package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Onboard;

@Repository
public interface OnboardRepository extends JpaRepository<Onboard, UUID> {
    List<Onboard> findAll();
}
