package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.Subscription;

public interface SubscriptionRepository extends JpaRepository<Subscription, Long> {

    Subscription findByNameAndStatusTrue(String name);

}
