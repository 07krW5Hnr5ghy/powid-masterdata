package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Membership;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MembershipRepository extends JpaRepository<Membership,Long> {
    Membership findByIdAndIdModule(Long id,Long idModule);
    List<Membership> findByIdAndIdModuleIn(Long id,List<Long> moduleList);
}
