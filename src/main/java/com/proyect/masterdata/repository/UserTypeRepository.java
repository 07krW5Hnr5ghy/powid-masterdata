package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.UserType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import java.util.List;

@Repository
public interface UserTypeRepository extends JpaRepository<UserType, Long> {
    List<UserType> findAllByStatusTrue();
    UserType findByIdAndStatusTrue(Long id);
    List<UserType> findByUserTypeIn(List<String> name);
    boolean existsByUserType(String userType);
}
