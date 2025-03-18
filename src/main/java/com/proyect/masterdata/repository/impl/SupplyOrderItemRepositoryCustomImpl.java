package com.proyect.masterdata.repository.impl;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.proyect.masterdata.domain.*;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.repository.SupplyOrderItemRepositoryCustom;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;

@Repository
public class SupplyOrderItemRepositoryCustomImpl implements SupplyOrderItemRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<SupplyOrderItem> searchForSupplyOrderItem(
            UUID clientId,
            Long orderNumber,
            String ref,
            String warehouse,
            String supplier,
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
        CriteriaQuery<SupplyOrderItem> criteriaQuery = criteriaBuilder.createQuery(SupplyOrderItem.class);
        Root<SupplyOrderItem> itemRoot = criteriaQuery.from(SupplyOrderItem.class);
        Join<SupplyOrderItem, SupplyOrder> supplyOrderItemSupplyOrderJoin = itemRoot.join("supplyOrder");
        Join<SupplyOrderItem, Product> supplyOrderItemProductJoin = itemRoot.join("product");
        Join<SupplyOrder, Warehouse> supplyOrderWarehouseJoin = supplyOrderItemSupplyOrderJoin.join("warehouse");
        Join<Product, Model> productModelJoin = supplyOrderItemProductJoin.join("model");
        Join<Product,Color> productColorJoin = supplyOrderItemProductJoin.join("color");
        Join<Product,Size> productSizeJoin = supplyOrderItemProductJoin.join("size");
        Join<SupplyOrder,Supplier> supplyOrderSupplierJoin = supplyOrderItemSupplyOrderJoin.join("supplier");

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                orderNumber,
                ref,
                warehouse,
                supplier,
                quantity,
                model,
                product,
                color,
                size,
                status,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                supplyOrderItemSupplyOrderJoin,
                supplyOrderItemProductJoin,
                supplyOrderWarehouseJoin,
                productModelJoin,
                productColorJoin,
                productSizeJoin,
                supplyOrderSupplierJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> purchaseItemList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                purchaseItemList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                purchaseItemList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(purchaseItemList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<SupplyOrderItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getSupplyOrderItemCount(
                clientId,
                orderNumber,
                ref,
                warehouse,
                supplier,
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
            String warehouse,
            String supplier,
            Integer quantity,
            String model,
            String product,
            String color,
            String size,
            Boolean status,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<SupplyOrderItem> itemRoot,
            Join<SupplyOrderItem, SupplyOrder> supplyOrderItemSupplyOrderJoin,
            Join<SupplyOrderItem, Product> supplyOrderItemProductJoin,
            Join<SupplyOrder, Warehouse> supplyOrderItemWarehouseJoin,
            Join<Product, Model> productModelJoin,
            Join<Product,Color> productColorJoin,
            Join<Product,Size> productSizeJoin,
            Join<SupplyOrder,Supplier> supplyOrderSupplierJoin
            ) {

        List<Predicate> conditions = new ArrayList<>();
        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }
        if(orderNumber!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(supplyOrderItemSupplyOrderJoin.get("orderNumber"), orderNumber)));
        }
        if(ref != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(supplyOrderItemSupplyOrderJoin.get("ref")),"%"+ref.toUpperCase()+"%"));
        }
        
        if(warehouse != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(supplyOrderItemWarehouseJoin.get("name")),"%"+warehouse.toUpperCase()+"%"));
        }

        if(quantity!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("quantity"), quantity)));
        }

        if(model!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productModelJoin.get("name")),"%"+model.toUpperCase()+"%"));
        }

        if(product!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(supplyOrderItemProductJoin.get("name")),"%"+product.toUpperCase()+"%"));
        }

        if(color!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productColorJoin.get("name")),"%"+color.toUpperCase()+"%"));
        }

        if(size!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productSizeJoin.get("name")),"%"+size.toUpperCase()+"%"));
        }

        if(supplier!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(supplyOrderSupplierJoin.get("businessName")),"%"+supplier.toUpperCase()+"%"));
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

        if(Boolean.TRUE.equals(status)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if(Boolean.FALSE.equals(status)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<SupplyOrderItem> itemRoot) {

        List<Order> supplyOrderItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("orderNumber")) {
            supplyOrderItemList.add(criteriaBuilder.asc(itemRoot.get("orderNumber")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            supplyOrderItemList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if(sortColumn.equalsIgnoreCase("updateDate")){
            supplyOrderItemList.add(criteriaBuilder.asc(itemRoot.get(
                    "updateDate"
            )));
        }

        if(sortColumn.equalsIgnoreCase("registrationDate")){
            supplyOrderItemList.add(criteriaBuilder.asc(itemRoot.get(
                    "registrationDate"
            )));
        }

        return supplyOrderItemList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<SupplyOrderItem> itemRoot) {

        List<Order> supplyOrderItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("orderNumber")) {
            supplyOrderItemList.add(criteriaBuilder.asc(itemRoot.get("orderNumber")));
        }

        if (sortColumn.equalsIgnoreCase("name")) {
            supplyOrderItemList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            supplyOrderItemList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if(sortColumn.equalsIgnoreCase("updateDate")){
            supplyOrderItemList.add(criteriaBuilder.asc(itemRoot.get(
                    "updateDate"
            )));
        }

        if(sortColumn.equalsIgnoreCase("registrationDate")){
            supplyOrderItemList.add(criteriaBuilder.asc(itemRoot.get(
                    "registrationDate"
            )));
        }

        return supplyOrderItemList;

    }

    private Long getSupplyOrderItemCount(
            UUID clientId,
            Long orderNumber,
            String ref,
            String warehouse,
            String supplier,
            Integer quantity,
            String model,
            String product,
            String color,
            String size,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<SupplyOrderItem> itemRoot = criteriaQuery.from(SupplyOrderItem.class);
        Join<SupplyOrderItem, SupplyOrder> supplyOrderItemSupplyOrderJoin = itemRoot.join("supplyOrder");
        Join<SupplyOrderItem, Product> supplyOrderItemProductJoin = itemRoot.join("product");
        Join<SupplyOrder, Warehouse> supplyOrderItemWarehouseJoin = supplyOrderItemSupplyOrderJoin.join("warehouse");
        Join<Product, Model> productModelJoin = supplyOrderItemProductJoin.join("model");
        Join<Product,Color> productColorJoin = supplyOrderItemProductJoin.join("color");
        Join<Product,Size> productSizeJoin = supplyOrderItemProductJoin.join("size");
        Join<SupplyOrder,Supplier> supplyOrderSupplierJoin = supplyOrderItemSupplyOrderJoin.join("supplier");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                orderNumber,
                ref,
                warehouse,
                supplier,
                quantity,
                model,
                product,
                color,
                size,
                status,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                supplyOrderItemSupplyOrderJoin,
                supplyOrderItemProductJoin,
                supplyOrderItemWarehouseJoin,
                productModelJoin,
                productColorJoin,
                productSizeJoin,
                supplyOrderSupplierJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
