package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.GeneralStock;
import com.proyect.masterdata.domain.Model;
import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.repository.GeneralStockRepositoryCustom;
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
public class GeneralStockRepositoryCustomImpl implements GeneralStockRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<GeneralStock> searchForGeneralStock(
            UUID clientId,
            String serial,
            String productSku,
            String model,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<GeneralStock> criteriaQuery = criteriaBuilder.createQuery(GeneralStock.class);
        Root<GeneralStock> itemRoot = criteriaQuery.from(GeneralStock.class);
        Join<GeneralStock, SupplierProduct> generalStockSupplierProductJoin = itemRoot.join("supplierProduct");
        Join<SupplierProduct, Product> supplierProductProductJoin = generalStockSupplierProductJoin.join("product");
        Join<Product, Model> productModelJoin = supplierProductProductJoin.join("model");
        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                serial,
                productSku,
                model,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                generalStockSupplierProductJoin,
                supplierProductProductJoin,
                productModelJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> generalStockList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                generalStockList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                generalStockList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(generalStockList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<GeneralStock> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                serial,
                productSku,
                model,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate
        );

        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);

    }

    private List<Predicate> predicate(
            UUID clientId,
            String serial,
            String productSku,
            String model,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<GeneralStock> itemRoot,
            Join<GeneralStock,SupplierProduct> generalStockSupplierProductJoin,
            Join<SupplierProduct,Product> supplierProductProductJoin,
            Join<Product,Model> productModelJoin) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if(serial != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(generalStockSupplierProductJoin.get("serial")),"%"+serial.toUpperCase()+"%"));
        }

        if(productSku != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(supplierProductProductJoin.get("sku")),"%"+productSku.toUpperCase()+"%"));
        }

        if(model != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productModelJoin.get("name")),"%"+model.toUpperCase()+"%"));
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

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<GeneralStock> itemRoot) {

        List<Order> generalStockList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            generalStockList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            generalStockList.add(criteriaBuilder.asc(itemRoot.get("supplierProductId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            generalStockList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            generalStockList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            generalStockList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            generalStockList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        return generalStockList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<GeneralStock> itemRoot) {

        List<Order> generalStockList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            generalStockList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            generalStockList.add(criteriaBuilder.desc(itemRoot.get("supplierProductId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            generalStockList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            generalStockList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            generalStockList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            generalStockList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        return generalStockList;

    }

    private Long getOrderCount(
            UUID clientId,
            String serial,
            String productSku,
            String model,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate
    ) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<GeneralStock> itemRoot = criteriaQuery.from(GeneralStock.class);
        Join<GeneralStock, SupplierProduct> generalStockSupplierProductJoin = itemRoot.join("supplierProduct");
        Join<SupplierProduct, Product> supplierProductProductJoin = generalStockSupplierProductJoin.join("product");
        Join<Product, Model> productModelJoin = supplierProductProductJoin.join("model");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                serial,
                productSku,
                model,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                generalStockSupplierProductJoin,
                supplierProductProductJoin,
                productModelJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
