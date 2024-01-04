package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.OnboardModule;

public interface OnboardModuleRepository extends JpaRepository<OnboardModule, Long> {
    List<OnboardModule> findByOnboardId(Long onboardId);
}
