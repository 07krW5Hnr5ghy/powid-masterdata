package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Membership;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface MembershipRepository extends JpaRepository<Membership, UUID> {
    Membership findByClientIdAndMembershipStateId(UUID clientId,UUID membershipStateId);
    Membership findByClientIdAndDemoTrue(UUID clientId);
}
