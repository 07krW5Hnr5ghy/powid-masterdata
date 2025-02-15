package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.proyect.masterdata.domain.Model;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.repository.ProductRepositoryCustom;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;

import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.PageImpl;

@Repository
public class ProductRepositoryCustomImpl implements ProductRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<Product> searchForProduct(
            UUID clientId,
            String sku,
            String model,
            List<UUID> brandIds,
            List<UUID> sizeIds,
            List<UUID> categoryProductIds,
            List<UUID> colorIds,
            List<UUID> unitIds,
            Boolean pictureFlag,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize, Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Product> criteriaQuery = criteriaBuilder.createQuery(Product.class);

        Root<Product> itemRoot = criteriaQuery.from(Product.class);
        Join<Product,Model> productModelJoin = itemRoot.join("model");

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicateConditions(
                clientId,
                sku,
                model,
                brandIds,
                sizeIds,
                categoryProductIds,
                colorIds,
                unitIds,
                pictureFlag,
                status,
                criteriaBuilder,
                itemRoot,
                productModelJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> productList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                productList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                productList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(productList);

        } else {

            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<Product> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                sku,
                model,
                brandIds,
                sizeIds,
                categoryProductIds,
                colorIds,
                unitIds,
                pictureFlag,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicateConditions(
            UUID clientId,
            String sku,
            String model,
            List<UUID> brandIds,
            List<UUID> sizeIds,
            List<UUID> categoryProductIds,
            List<UUID> colorIds,
            List<UUID> unitIds,
            Boolean pictureFlag,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<Product> itemRoot,
            Join<Product,Model> productModelJoin) {

        List<Predicate> conditions = new ArrayList<>();

        if(sku != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(itemRoot.get("sku")),"%"+sku.toUpperCase()+"%"));
        }

        if (model != null) {
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productModelJoin.get("name")),"%"+model.toUpperCase()+"%"));
        }

        if(!brandIds.isEmpty()){
            conditions.add(criteriaBuilder.and(productModelJoin.get("brandId").in(brandIds)));
        }

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if(!sizeIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("sizeId").in(sizeIds)));
        }

        if(!categoryProductIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("categoryProductId").in(categoryProductIds)));
        }

        if(!colorIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("colorId").in(colorIds)));
        }

        if(!unitIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("unitId").in(unitIds)));
        }

        if (pictureFlag != null) {
            conditions.add(criteriaBuilder.equal(itemRoot.get("pictureFlag"), pictureFlag));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Product> itemRoot) {

        List<Order> productList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("sku")) {
            productList.add(criteriaBuilder.asc(itemRoot.get("sku")));
        }

        if (sortColumn.equalsIgnoreCase("modelId")) {
            productList.add(criteriaBuilder.asc(itemRoot.get("modelId")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            productList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        return productList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Product> itemRoot) {

        List<Order> productList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("sku")) {
            productList.add(criteriaBuilder.desc(itemRoot.get("sku")));
        }

        if (sortColumn.equalsIgnoreCase("modelId")) {
            productList.add(criteriaBuilder.desc(itemRoot.get("modelId")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            productList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        return productList;
    }

    private Long getOrderCount(
            UUID clientId,
            String sku,
            String model,
            List<UUID> brandIds,
            List<UUID> sizeIds,
            List<UUID> categoryProductIds,
            List<UUID> colorIds,
            List<UUID> unitIds,
            Boolean pictureFlag,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Product> itemRoot = criteriaQuery.from(Product.class);
        Join<Product,Model> productModelJoin = itemRoot.join("model");

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                clientId,
                sku,
                model,
                brandIds,
                sizeIds,
                categoryProductIds,
                colorIds,
                unitIds,
                pictureFlag,
                status,
                criteriaBuilder,
                itemRoot,
                productModelJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
