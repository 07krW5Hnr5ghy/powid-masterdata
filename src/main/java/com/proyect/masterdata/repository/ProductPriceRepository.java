package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ProductPrice;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface ProductPriceRepository extends JpaRepository<ProductPrice, UUID> {
    ProductPrice findByProductId(UUID id);
    ProductPrice findByProductIdAndStatusTrue(UUID id);
}
