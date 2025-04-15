package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ProductPrice;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface ProductPriceRepository extends JpaRepository<ProductPrice, UUID> {
    ProductPrice findByProductId(UUID id);
    ProductPrice findByProductIdAndStatusTrue(UUID id);
    @Query(value = """
    SELECT * FROM marketing.product_price as pp 
    WHERE pp.product_id = :productId 
    ORDER BY ABS(EXTRACT(EPOCH FROM (:registrationDate - pp.registration_date))) ASC 
    LIMIT 1
    """, nativeQuery = true)
    ProductPrice findClosestByProductIdAndDate(@Param("productId") UUID productId,
                                               @Param("registrationDate") OffsetDateTime registrationDate);
}
