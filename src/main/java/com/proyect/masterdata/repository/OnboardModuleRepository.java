package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.OnboardModule;

public interface OnboardModuleRepository extends JpaRepository<OnboardModule, UUID> {
    List<OnboardModule> findByOnboardId(UUID onboardId);
}
