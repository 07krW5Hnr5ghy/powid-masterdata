package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.MembershipType;
import org.springframework.data.jpa.repository.JpaRepository;

public interface MembershipTypeRepository extends JpaRepository<MembershipType,Long> {
}
