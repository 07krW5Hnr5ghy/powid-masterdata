package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SaleChannel;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface SaleChannelRepository extends JpaRepository<SaleChannel,Long> {
    List<SaleChannel> findAllByStatusTrue();
    List<SaleChannel> findAllByStatusFalse();
    SaleChannel findByIdAndStatusTrue(Long id);
    SaleChannel findByNameAndStatusTrue(String name);
    List<SaleChannel> findByUser(String user);
    void deleteByIdAndUser(Long id, String User);
}
