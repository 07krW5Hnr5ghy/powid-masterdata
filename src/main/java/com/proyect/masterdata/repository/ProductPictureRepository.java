package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ProductPicture;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ProductPictureRepository extends JpaRepository<ProductPicture,Long> {
    List<ProductPicture> findAllByProductId(Long orderId);
}
