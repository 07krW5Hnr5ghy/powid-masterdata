package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SaleChannel;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface SaleChannelRepository extends JpaRepository<SaleChannel, Long> {
    List<SaleChannel> findAllByStatusTrue();
    List<SaleChannel> findAllByStatusFalse();
    SaleChannel findByIdAndStatusTrue(Long id);
    SaleChannel findByNameAndStatusTrue(String name);
    SaleChannel findByNameAndStatusFalse(String name);
    List<SaleChannel> findByNameIn(List<String> names);
    SaleChannel findByName(String name);
}
