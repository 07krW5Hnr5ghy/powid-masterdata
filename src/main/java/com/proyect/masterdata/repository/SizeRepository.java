package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Size;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface SizeRepository extends JpaRepository<Size,Long> {
    List<Size> findAllByStatusTrue();
    List<Size> findAllByStatusFalse();
    Size findByIdAndStatusTrue(Long id);
    Size findByNameAndStatusTrue(String name);
    List<Size> findByUser(String user);
    void deleteByIdAndUser(Long id, String User);
}
