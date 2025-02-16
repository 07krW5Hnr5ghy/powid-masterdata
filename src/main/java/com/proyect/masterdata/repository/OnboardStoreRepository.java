package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.OnboardStore;

import java.util.UUID;

@Repository
public interface OnboardStoreRepository extends JpaRepository<OnboardStore, UUID> {

}
