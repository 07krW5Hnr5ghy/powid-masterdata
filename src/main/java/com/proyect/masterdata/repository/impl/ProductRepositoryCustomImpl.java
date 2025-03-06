package com.proyect.masterdata.repository.impl;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.proyect.masterdata.domain.*;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

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
            String productSku,
            String product,
            String model,
            String brand,
            String size,
            String categoryProduct,
            String subCategoryProduct,
            String color,
            String unit,
            Boolean pictureFlag,
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
        CriteriaQuery<Product> criteriaQuery = criteriaBuilder.createQuery(Product.class);

        Root<Product> itemRoot = criteriaQuery.from(Product.class);
        Join<Product,Model> productModelJoin = itemRoot.join("model");
        Join<Product, Size> productSizeJoin = itemRoot.join("size");
        Join<Product, SubCategoryProduct> productSubCategoryProductJoin = itemRoot.join("subCategoryProduct");
        Join<Product,Unit> productUnitJoin = itemRoot.join("unit");
        Join<Product,Color> productColorJoin = itemRoot.join("color");
        Join<Model, Brand> modelBrandJoin = productModelJoin.join("brand");
        Join<SubCategoryProduct,CategoryProduct> subCategoryProductCategoryProductJoin = productSubCategoryProductJoin.join("categoryProduct");


        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicateConditions(
                clientId,
                productSku,
                product,
                model,
                brand,
                size,
                categoryProduct,
                subCategoryProduct,
                color,
                unit,
                pictureFlag,
                status,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                productModelJoin,
                productSizeJoin,
                productSubCategoryProductJoin,
                productUnitJoin,
                productColorJoin,
                modelBrandJoin,
                subCategoryProductCategoryProductJoin);

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
                productSku,
                product,
                model,
                brand,
                size,
                categoryProduct,
                subCategoryProduct,
                color,
                unit,
                pictureFlag,
                status,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicateConditions(
            UUID clientId,
            String productSku,
            String product,
            String model,
            String brand,
            String size,
            String categoryProduct,
            String subCategoryProduct,
            String color,
            String unit,
            Boolean pictureFlag,
            Boolean status,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<Product> itemRoot,
            Join<Product,Model> productModelJoin,
            Join<Product, Size> productSizeJoin,
            Join<Product, SubCategoryProduct> productSubCategoryProductJoin,
            Join<Product,Unit> productUnitJoin,
            Join<Product,Color> productColorJoin,
            Join<Model, Brand> modelBrandJoin,
            Join<SubCategoryProduct,CategoryProduct> subCategoryProductCategoryProductJoin
    ){

        List<Predicate> conditions = new ArrayList<>();

        if(productSku != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(itemRoot.get("sku")),"%"+productSku.toUpperCase()+"%"));
        }

        if(product != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(itemRoot.get("name")),"%"+product+"%"));
        }

        if (model != null) {
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productModelJoin.get("name")),"%"+model.toUpperCase()+"%"));
        }

        if (brand != null) {
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productModelJoin.get("name")),"%"+brand.toUpperCase()+"%"));
        }

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (size != null) {
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productSizeJoin.get("name")),"%"+size.toUpperCase()+"%"));
        }

        if (subCategoryProduct != null) {
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productSubCategoryProductJoin.get("name")),"%"+subCategoryProduct.toUpperCase()+"%"));
        }

        if (categoryProduct != null) {
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(subCategoryProductCategoryProductJoin.get("name")),"%"+categoryProduct.toUpperCase()+"%"));
        }

        if (color != null) {
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productColorJoin.get("name")),"%"+color.toUpperCase()+"%"));
        }

        if (unit != null) {
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productUnitJoin.get("name")),"%"+unit.toUpperCase()+"%"));
        }

        if (pictureFlag != null) {
            conditions.add(criteriaBuilder.equal(itemRoot.get("pictureFlag"), pictureFlag));
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
            String productSku,
            String product,
            String model,
            String brand,
            String size,
            String categoryProduct,
            String subCategoryProduct,
            String color,
            String unit,
            Boolean pictureFlag,
            Boolean status,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Product> itemRoot = criteriaQuery.from(Product.class);
        Join<Product,Model> productModelJoin = itemRoot.join("model");
        Join<Product, Size> productSizeJoin = itemRoot.join("size");
        Join<Product, SubCategoryProduct> productSubCategoryProductJoin = itemRoot.join("subCategoryProduct");
        Join<Product,Unit> productUnitJoin = itemRoot.join("unit");
        Join<Product,Color> productColorJoin = itemRoot.join("color");
        Join<Model, Brand> modelBrandJoin = productModelJoin.join("brand");
        Join<SubCategoryProduct,CategoryProduct> subCategoryProductCategoryProductJoin = productSubCategoryProductJoin.join("categoryProduct");

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                clientId,
                productSku,
                product,
                model,
                brand,
                size,
                categoryProduct,
                subCategoryProduct,
                color,
                unit,
                pictureFlag,
                status,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                productModelJoin,
                productSizeJoin,
                productSubCategoryProductJoin,
                productUnitJoin,
                productColorJoin,
                modelBrandJoin,
                subCategoryProductCategoryProductJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
