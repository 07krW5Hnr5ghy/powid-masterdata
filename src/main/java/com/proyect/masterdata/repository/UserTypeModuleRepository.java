package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.UserTypeModule;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface UserTypeModuleRepository extends JpaRepository<UserTypeModule,Long> {
    UserTypeModule findByUserTypeAndStatusTrue(String name);
}
