package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Subscription;

@Repository
public interface SubscriptionRepository extends JpaRepository<Subscription, UUID> {
    Subscription findByNameAndStatusTrue(String name);
    Subscription findByNameAndStatusFalse(String name);
    Boolean existsByNameAndStatusTrue(String name);
    List<Subscription> findAllByStatusTrue();

}
