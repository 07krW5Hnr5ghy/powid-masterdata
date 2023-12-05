package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.OnboardChannel;

@Repository
public interface OnboardChannelRepository extends JpaRepository<OnboardChannel, Long> {
    List<OnboardChannel> findByOnboardId(Long onboardId);
}
