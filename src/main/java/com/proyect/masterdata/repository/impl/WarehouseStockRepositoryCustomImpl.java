package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.List;

import com.proyect.masterdata.domain.*;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.repository.WarehouseStockRepositoryCustom;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;

@Repository
public class WarehouseStockRepositoryCustomImpl implements WarehouseStockRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<WarehouseStock> searchForWarehouseStock(
            Long clientId,
            List<Long> warehouseIds,
            String serial,
            String productSku,
            String model,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<WarehouseStock> criteriaQuery = criteriaBuilder.createQuery(WarehouseStock.class);
        Root<WarehouseStock> itemRoot = criteriaQuery.from(WarehouseStock.class);
        Join<WarehouseStock,SupplierProduct> warehouseStockSupplierProductJoin = itemRoot.join("supplierProduct");
        Join<SupplierProduct, Product> supplierProductProductJoin = warehouseStockSupplierProductJoin.join("product");
        Join<Product, Model> productModelJoin =supplierProductProductJoin.join("model");

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                warehouseIds,
                serial,
                productSku,
                model,
                clientId,
                criteriaBuilder,
                itemRoot,
                warehouseStockSupplierProductJoin,
                supplierProductProductJoin,
                productModelJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> warehouseStockList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                warehouseStockList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                warehouseStockList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(warehouseStockList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<WarehouseStock> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                warehouseIds,
                serial,
                productSku,
                model,
                clientId);

        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);

    }

    private List<Predicate> predicate(
            List<Long> warehouseIds,
            String serial,
            String productSku,
            String model,
            Long clientId,
            CriteriaBuilder criteriaBuilder,
            Root<WarehouseStock> itemRoot,
            Join<WarehouseStock,SupplierProduct> warehouseStockSupplierProductJoin,
            Join<SupplierProduct,Product> supplierProductProductJoin,
            Join<Product,Model> productModelJoin) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if(!warehouseIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("warehouseId").in(warehouseIds)));
        }

        if(serial != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(warehouseStockSupplierProductJoin.get("serial")),"%"+serial.toUpperCase()+"%"));
        }

        if(productSku != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(supplierProductProductJoin.get("sku")),"%"+productSku.toUpperCase()+"%"));
        }

        if(model != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productModelJoin.get("name")),"%"+model.toUpperCase()+"%"));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<WarehouseStock> itemRoot) {

        List<Order> warehouseStockList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            warehouseStockList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            warehouseStockList.add(criteriaBuilder.asc(itemRoot.get("warehouseId")));
        }

        return warehouseStockList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<WarehouseStock> itemRoot) {

        List<Order> warehouseStockList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            warehouseStockList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            warehouseStockList.add(criteriaBuilder.desc(itemRoot.get("warehouseId")));
        }

        return warehouseStockList;

    }

    private Long getOrderCount(
            List<Long> warehouseIds,
            String serial,
            String productSku,
            String model,
            Long clientId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<WarehouseStock> itemRoot = criteriaQuery.from(WarehouseStock.class);
        Join<WarehouseStock,SupplierProduct> warehouseStockSupplierProductJoin = itemRoot.join("supplierProduct");
        Join<SupplierProduct, Product> supplierProductProductJoin = warehouseStockSupplierProductJoin.join("product");
        Join<Product, Model> productModelJoin =supplierProductProductJoin.join("model");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                warehouseIds,
                serial,
                productSku,
                model,
                clientId,
                criteriaBuilder,
                itemRoot,
                warehouseStockSupplierProductJoin,
                supplierProductProductJoin,
                productModelJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
