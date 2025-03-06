package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.repository.WarehouseOutputItemRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Repository
public class WarehouseOutputItemRepositoryCustomImpl implements WarehouseOutputItemRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<WarehouseOutputItem> searchForWarehouseOutputItem(
            UUID clientId,
            Long orderNumber,
            String ref,
            String courier,
            String warehouse,
            Integer quantity,
            String model,
            String product,
            String color,
            String size, 
            OffsetDateTime registrationStartDate, 
            OffsetDateTime registrationEndDate, 
            OffsetDateTime updateStartDate, 
            OffsetDateTime updateEndDate, 
            String sort, 
            String sortColumn, 
            Integer pageNumber, 
            Integer pageSize, 
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<WarehouseOutputItem> criteriaQuery = criteriaBuilder.createQuery(WarehouseOutputItem.class);
        Root<WarehouseOutputItem> itemRoot = criteriaQuery.from(WarehouseOutputItem.class);
        Join<WarehouseOutputItem,WarehouseOutput> warehouseOutputItemWarehouseOutputJoin = itemRoot.join("warehouseOutput");
        Join<WarehouseOutput, Courier> warehouseOutputCourierJoin = warehouseOutputItemWarehouseOutputJoin.join("courier");
        Join<WarehouseOutput, Warehouse> warehouseOutputItemWarehouseJoin = warehouseOutputItemWarehouseOutputJoin.join("warehouse");
        Join<WarehouseOutputItem, Product> warehouseOutputItemProductJoin = itemRoot.join("product");
        Join<Product,Model> productModelJoin = warehouseOutputItemProductJoin.join("model");
        Join<Product,Color> productColorJoin = warehouseOutputItemProductJoin.join("color");
        Join<Product,Size> productSizeJoin = warehouseOutputItemProductJoin.join("size");
        List<Predicate> conditions = predicate(
                clientId,
                orderNumber,
                ref,
                courier,
                warehouse,
                quantity,
                 model,
                product,
                color,
                size,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot,
                warehouseOutputItemWarehouseOutputJoin,
                warehouseOutputCourierJoin,
                warehouseOutputItemWarehouseJoin,
                warehouseOutputItemProductJoin,
                productModelJoin,
                productColorJoin,
                productSizeJoin
        );
        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> warehouseOutputList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                warehouseOutputList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                warehouseOutputList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(warehouseOutputList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }
        TypedQuery<WarehouseOutputItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getWarehouseOutputItemCount(
                clientId,
                orderNumber,
                ref,
                courier,
                warehouse,
                quantity,
                model,
                product,
                color,
                size,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }
    private List<Predicate> predicate(
            UUID clientId,
            Long orderNumber,
            String ref,
            String courier,
            String warehouse,
            Integer quantity,
            String model,
            String product,
            String color,
            String size,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<WarehouseOutputItem> itemRoot,
            Join<WarehouseOutputItem,WarehouseOutput> warehouseOutputItemWarehouseOutputJoin,
            Join<WarehouseOutput, Courier> warehouseOutputCourierJoin,
            Join<WarehouseOutput, Warehouse> warehouseOutputWarehouseJoin,
            Join<WarehouseOutputItem, Product> warehouseOutputItemProductJoin,
            Join<Product,Model> productModelJoin,
            Join<Product,Color> productColorJoin,
            Join<Product,Size> productSizeJoin
    ){
        List<Predicate> conditions = new ArrayList<>();
        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }
        if(orderNumber!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(warehouseOutputItemWarehouseOutputJoin.get("orderNumber"), orderNumber)));
        }
        if(ref != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(warehouseOutputItemWarehouseOutputJoin.get("ref")),"%"+ref.toUpperCase()+"%"));
        }

        if(courier != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(warehouseOutputCourierJoin.get("name")),"%"+courier.toUpperCase()+"%"));
        }

        if(warehouse != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(warehouseOutputWarehouseJoin.get("name")),"%"+warehouse.toUpperCase()+"%"));
        }
        
        if(quantity!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("quantity"), quantity)));
        }
        
        if(model!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productModelJoin.get("name")),"%"+model.toUpperCase()+"%"));
        }

        if(product!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(warehouseOutputItemProductJoin.get("name")),"%"+product.toUpperCase()+"%"));
        }

        if(color!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productColorJoin.get("name")),"%"+color.toUpperCase()+"%"));
        }

        if(size!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productSizeJoin.get("name")),"%"+size.toUpperCase()+"%"));
        }

        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<WarehouseOutputItem> itemRoot) {

        List<Order> warehouseOutputItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("orderNumber")) {
            warehouseOutputItemList.add(criteriaBuilder.asc(itemRoot.get("orderNumber")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            warehouseOutputItemList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if(sortColumn.equalsIgnoreCase("updateDate")){
            warehouseOutputItemList.add(criteriaBuilder.asc(itemRoot.get(
                    "updateDate"
            )));
        }

        if(sortColumn.equalsIgnoreCase("registrationDate")){
            warehouseOutputItemList.add(criteriaBuilder.asc(itemRoot.get(
                    "registrationDate"
            )));
        }

        return warehouseOutputItemList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<WarehouseOutputItem> itemRoot) {

        List<Order> warehouseOutputItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("orderNumber")) {
            warehouseOutputItemList.add(criteriaBuilder.asc(itemRoot.get("orderNumber")));
        }

        if (sortColumn.equalsIgnoreCase("name")) {
            warehouseOutputItemList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            warehouseOutputItemList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if(sortColumn.equalsIgnoreCase("updateDate")){
            warehouseOutputItemList.add(criteriaBuilder.asc(itemRoot.get(
                    "updateDate"
            )));
        }

        if(sortColumn.equalsIgnoreCase("registrationDate")){
            warehouseOutputItemList.add(criteriaBuilder.asc(itemRoot.get(
                    "registrationDate"
            )));
        }

        return warehouseOutputItemList;

    }

    private Long getWarehouseOutputItemCount(
            UUID clientId,
            Long orderNumber,
            String ref,
            String courier,
            String warehouse,
            Integer quantity,
            String model,
            String product,
            String color,
            String size,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status
    ){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<WarehouseOutputItem> itemRoot = criteriaQuery.from(WarehouseOutputItem.class);
        Join<WarehouseOutputItem,WarehouseOutput> warehouseOutputItemWarehouseOutputJoin = itemRoot.join("warehouseOutput");
        Join<WarehouseOutput, Courier> warehouseOutputCourierJoin = warehouseOutputItemWarehouseOutputJoin.join("courier");
        Join<WarehouseOutput, Warehouse> warehouseOutputItemWarehouseJoin = warehouseOutputItemWarehouseOutputJoin.join("warehouse");
        Join<WarehouseOutputItem, Product> warehouseOutputItemProductJoin = itemRoot.join("product");
        Join<Product,Model> productModelJoin = warehouseOutputItemProductJoin.join("model");
        Join<Product,Color> productColorJoin = warehouseOutputItemProductJoin.join("color");
        Join<Product,Size> productSizeJoin = warehouseOutputItemProductJoin.join("size");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                orderNumber,
                ref,
                courier,
                warehouse,
                quantity,
                model,
                product,
                color,
                size,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot,
                warehouseOutputItemWarehouseOutputJoin,
                warehouseOutputCourierJoin,
                warehouseOutputItemWarehouseJoin,
                warehouseOutputItemProductJoin,
                productModelJoin,
                productColorJoin,
                productSizeJoin
        );
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
