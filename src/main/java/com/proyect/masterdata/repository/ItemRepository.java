package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Item;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ItemRepository extends JpaRepository<Item,Long> {
    List<Item> findAllByOrderId(Long orderId);
    Item findByIdAndOrderId(Long itemId,Long orderId);
}
