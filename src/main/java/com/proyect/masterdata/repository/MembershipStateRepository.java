package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.MembershipState;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface MembershipStateRepository extends JpaRepository<MembershipState, UUID> {
    MembershipState findByName(String name);
    MembershipState findByNameAndStatusTrue(String name);
    MembershipState findByNameAndStatusFalse(String name);
}
