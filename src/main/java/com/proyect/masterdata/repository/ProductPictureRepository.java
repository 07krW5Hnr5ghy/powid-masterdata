package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ProductPicture;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Set;
import java.util.UUID;

@Repository
public interface ProductPictureRepository extends JpaRepository<ProductPicture, UUID> {
    List<ProductPicture> findAllByProductId(UUID orderId);
    List<ProductPicture> findAlByClientIdAndProductId(UUID clientId,UUID productId);
    List<ProductPicture> findAllByClientIdAndProductIdIn (UUID clientId, Set<UUID> productIds);
}
