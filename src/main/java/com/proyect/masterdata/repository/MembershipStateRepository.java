package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.MembershipState;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MembershipStateRepository extends JpaRepository<MembershipState,Long> {
    MembershipState findByName(String name);
    MembershipState findByNameAndStatusTrue(String name);
}
