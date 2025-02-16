package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.MembershipModule;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface MembershipModuleRepository extends JpaRepository<MembershipModule, UUID> {
}
