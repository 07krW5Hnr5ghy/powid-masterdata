package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ProductPrice;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ProductPriceRepository extends JpaRepository<ProductPrice,Long> {
    ProductPrice findByProductId(Long id);
}
