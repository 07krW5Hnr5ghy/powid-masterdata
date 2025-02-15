package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CourierPicture;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface CourierPictureRepository extends JpaRepository<CourierPicture, UUID> {
    List<CourierPicture> findAllByOrderId(UUID orderId);
}
